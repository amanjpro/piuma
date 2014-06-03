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
    def macroTransform(annottees: Any*): Any= macro phaseMacro.phaseImpl
  }


  object phaseMacro {
    
    def phaseImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          
          // Cannot extend anything except AnyRef, and you should
          // remove it
          checkParents(c)(clazz.impl.parents)
          val nbody = clazz.impl.body.foldLeft(List[Tree]())((z, y) => {
            y match {
              // This case is to bypass the limitations of DefMacro
              case Apply(Ident(TermName("name")), 
                List(str @ (Literal(Constant(x: String))))) =>
                q"val phaseName: java.lang.String = ${str}" :: z
              case DefDef(_, n, _, _, _, _) if(n == termNames.CONSTRUCTOR) =>
                z
              case _ =>
                y :: z
            }
          })
          // ClassDef(clazz.mods, clazz.name, clazz.tparams, nimpl)

          q"""
          class ${clazz.name}
          (val plgn: ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin)
          extends 
          ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent(
              plgn) {
            import global._
            ..${nbody}
          }
          """
        case x => 
          fail(c, "@phase can only be applied on classes")
          EmptyTree
      }


      c.Expr[Any](expandee)
    }
  }


  // plugin macro
  class plugin(n: Any*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro pluginMacro.pluginImpl
  }

  object pluginMacro {
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

