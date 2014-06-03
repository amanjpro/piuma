package ch.usi.inf.l3.lombrello.neve


import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.tailrec
import scala.reflect.runtime.universe._


object NeveDSL {


  // Helper functions
  private def fail(c: Context, msg: String): Nothing = {
    c.abort(c.enclosingPosition, msg)
  }


  private def checkParents(c: Context)
           (parents: List[c.universe.Tree]): Unit = {
    // I convert them to String, because pattern matching does not work
    // properly for Trees
    parents.map(_.toString) match {
      case "scala.AnyRef" :: Nil => ()
      case _ => fail(c, "Phase classes cannot extend and/or "+
                "mix in anything other than AnyRef\n" +
                "       expected: List(scala.AnyRef)\n" +
                "          found: " + parents)
    }
  }




  // phase macro
  class phase extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any= macro Macros.phaseImpl
  }

  // plugin macro
  class plugin(n: Any*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Macros.pluginImpl
  }


  
  object Macros {
    
    def phaseImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          
          // Cannot extend anything except AnyRef, and you should
          // remove it
          checkParents(c)(clazz.impl.parents)

          val nameTree: Tree = c.prefix.tree match {
            case Apply(_, x :: Nil) =>
                x match {
                  case str @ Literal(Constant(_)) =>
                    q"val phaseName: java.lang.String = ${str}"
                  case _ =>
                    fail(c, "@phase should provide a name of type String")
                    EmptyTree
                }
            case _ =>
              fail(c, "@phase should provide a single name")
              EmptyTree
          }

          val tbody1 = nameTree :: clazz.impl.body

          val runsRightAfter = tbody1.foldLeft(None: Option[Tree])((z, y) => {
            y match {
              case Apply(Ident(TermName("rightAfter")), List(x)) =>
                Some(x)
              case _ => z
            }
          })





          // If rightAfter is declared, after won't be necessary
          val hasRunsAfter = tbody1.foldLeft(false)((z, y) => {
            y match {
              case Apply(Ident(TermName("after")), List(x)) =>
                true
              case _ => z
            }
          })


          val tbody2 = if(runsRightAfter != None && !hasRunsAfter) {
                         q"after(List(${runsRightAfter.get}))" :: tbody1
                       } else {
                         tbody1
                       }

          val nbody = tbody2.foldLeft(List[Tree]())((z, y) => {
            y match {
              case DefDef(_, TermName("transform"), Nil,
                  List(List(x)), tpt, rhs) =>
                q"""
                final override def newTransformer(unit: CompilationUnit):
                    Transformer = new TransformerComponent(unit) {
                  final override def transform(${x.name}: ${x.tpt}): 
                        ${tpt} = ${rhs}
                }
                """ :: z
              case Apply(Ident(TermName("rightAfter")), List(x)) =>
                q"override val runsRightAfter: Option[String] = Some(${x})" :: z
              case Apply(Ident(TermName("after")), List(x)) =>
                q"val runsAfter: List[String] = ${x}" :: z
              case Apply(Ident(TermName("before")), List(x)) =>
                q"override val runsBefore: List[String] = ${x}" :: z
              case DefDef(_, n, _, _, _, _) if(n == termNames.CONSTRUCTOR) =>
                z
              case _ =>
                y :: z
            }
          })

          q"""
          class ${clazz.name}
          (val plgn: ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin)
          extends 
          ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent(
              plgn) {
            import global._
            import plgn._
            ..${nbody}
          }
          """
        case x => 
          fail(c, "@phase can only be applied on classes")
          EmptyTree
      }


      c.Expr[Any](expandee)
    }


    def pluginImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          // Cannot extend anything except AnyRef, and you should
          // remove it
          checkParents(c)(clazz.impl.parents)

          val params: List[Tree] = c.prefix.tree match {
            case Apply(_, xs) =>
              xs.map((x) => {
                val id = Ident(TypeName(x.toString))
                q"new ${id}(this)"
              })
            case _ =>
              fail(c, "@plugin should define at least one plugin")
              Nil
          }


          val components = q"""val pluginComponents: 
           List[
           ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent] = 
                 List(..${params})"""

          val tbody = components :: clazz.impl.body
          val nbody = tbody.foldLeft(List[Tree]())((z, y) => {

            y match {
              // This case is to bypass the limitations of DefMacro
              case Apply(Ident(TermName("describe")), 
                List(str @ (Literal(Constant(x: String))))) =>
                q"override val description: java.lang.String = ${str}" :: z
              case DefDef(_, n, _, _, _, _) if(n == termNames.CONSTRUCTOR) =>
                z
              case _ =>
                y :: z
            }
          })
          q"""
            class ${clazz.name}(override val global: 
                      scala.tools.nsc.Global) extends 
            ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin(global) {
              ..${nbody}
            }
          """
          // ClassDef(clazz.mods, clazz.name, clazz.tparams, nimpl)
        case x => 
          fail(c, "@plugin can only be applied on classes")
          EmptyTree
      }


      c.Expr[Any](expandee)
    }

    
  }


  
}

