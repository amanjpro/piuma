/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.neve

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.tailrec
import scala.reflect.runtime.universe._


object NeveDSL {

  
  // phase macro
  class phase extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any= macro Macros.phaseImpl
  }

  // plugin macro
  class plugin(n: Any*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Macros.pluginImpl
  }

  // checker macro
  class checker extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any= macro Macros.checkerImpl
  }
  
  object Macros {
    
    def checkerImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          
          // Cannot extend anything except AnyRef, and you should
          // remove it
          checkParents(c)(clazz.impl.parents)


          val nbody = generateBody(c)(clazz.impl.body, true)
                    
          q"""
          class ${clazz.name}
          (override val plgn: ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin)
          extends 
          ch.usi.inf.l3.lombrello.plugin.CheckerPluginComponent(
              plgn) {
            import plgn._
            import plgn.global._
            import plgn.utilities._
            ..${nbody}
          }
          """
        case x => 
          fail(c, "@checker can only be applied on classes")
          EmptyTree
      }


      c.Expr[Any](expandee)
    }

    def phaseImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          
          // Cannot extend anything except AnyRef, and you should
          // remove it
          checkParents(c)(clazz.impl.parents)


          val nbody = generateBody(c)(clazz.impl.body, false)
                    
          val r = q"""
          class ${clazz.name}
          (override val plgn: ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin)
          extends 
          ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent(
              plgn) {
            import plgn._
            import plgn.global._
            import plgn.utilities._
            ..${nbody}
          }
          """
          r
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
           List[scala.tools.nsc.plugins.PluginComponent] = 
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
            ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin(global) {
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


  private def getNameTree(c: Context, caller: String): c.universe.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, x :: Nil) =>
        x match {
          case str @ Literal(Constant(_)) =>
            q"val phaseName: java.lang.String = ${str}"
          case _ =>
            fail(c, s"${caller} should provide a name of type String")
            EmptyTree
        }
      case _ =>
        fail(c, s"${caller} should provide a single name")
        EmptyTree
    }
  }

  private def extractRightAfter(c: Context)
            (body: List[c.universe.Tree]): Option[c.universe.Tree] = {
    import c.universe._
    body.foldLeft(None: Option[Tree])((z, y) => {
      y match {
        case Apply(Ident(TermName("rightAfter")), List(x)) =>
          Some(x)
        case _ => z
      }
    })
  }


  private def doesHaveAfter(c: Context)
        (body: List[c.universe.Tree]): Boolean = {
    import c.universe._
    body.foldLeft(false)((z, y) => {
      y match {
        case Apply(Ident(TermName("after")), List(x)) =>
          true
        case _ => z
      }
    })

  }


  private def splitInnerAndOuterBody(c: Context)(body: List[c.universe.Tree],
        isChecker: Boolean,
        outerBody: List[c.universe.Tree],
        innerBody: List[c.universe.Tree]): 
        (List[c.universe.Tree], List[c.universe.Tree]) = {

    import c.universe._

    body match {
      case (x @ DefDef(_, TermName("check"), Nil,
        List(List(_)), _, _)) :: xs if isChecker =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case (x @ DefDef(_, TermName("transform"), Nil,
        List(List(_)), _, _)) :: xs if !isChecker =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case (x @ Apply(Ident(TermName("rightAfter")), List(_))) :: xs =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case (x @ Apply(Ident(TermName("after")), List(_))) :: xs =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case (x @ Apply(Ident(TermName("before")), List(_))) :: xs =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case DefDef(_, n, _, _, _, _) :: xs if(n == termNames.CONSTRUCTOR) =>
        splitInnerAndOuterBody(c)(xs, isChecker, outerBody, innerBody)
      case (x @ DefDef(mods, _, _, _, _, _)) :: xs 
                                              if !mods.hasFlag(Flag.PRIVATE) &&
                                              !mods.hasFlag(Flag.PROTECTED) && 
                                              !mods.hasFlag(Flag.OVERRIDE) =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case (x @ ValDef(mods, _, _, _)) :: xs if !mods.hasFlag(Flag.PRIVATE) &&
                                              !mods.hasFlag(Flag.PROTECTED) && 
                                              !mods.hasFlag(Flag.OVERRIDE) =>
        splitInnerAndOuterBody(c)(xs, isChecker, x :: outerBody, innerBody)
      case x :: xs =>
        splitInnerAndOuterBody(c)(xs, isChecker, outerBody, x :: innerBody)
      case Nil =>
        (outerBody.reverse, innerBody.reverse)
    }
  }

  private def bodyMacros(c: Context)(body: List[c.universe.Tree],
        isChecker: Boolean,
        innerBody: List[c.universe.Tree],
        collected: List[c.universe.Tree]): List[c.universe.Tree] = {
    import c.universe._

    body match {
      case DefDef(_, TermName("check"), Nil,
        List(List(x)), tpt, rhs) :: xs if isChecker =>
        val translated = q"""
        final def newPhase(_prev: scala.tools.nsc.Phase): StdPhase = 
          new CheckerComponent(_prev) {
            ..${innerBody}
            final def apply(${x.name}: ${x.tpt}): ${tpt} = ${rhs}
          }
        """
        bodyMacros(c)(xs, isChecker, Nil, translated :: collected)
      case DefDef(_, TermName("transform"), Nil,
        List(List(x)), tpt, rhs) :: xs if !isChecker =>
        val translated = q"""
        final override def newTransformer(unit: CompilationUnit):
          Transformer = new TransformerComponent(unit) {
            ..${innerBody}
            final override def transform(${x.name}: ${x.tpt}): ${tpt} = ${rhs}
          }
        """
        bodyMacros(c)(xs, isChecker, Nil, translated :: collected)
      case Apply(Ident(TermName("rightAfter")), List(x)) :: xs =>
        val translated = 
          q"override val runsRightAfter: Option[String] = Some(${x})"
        bodyMacros(c)(xs, isChecker, innerBody, translated :: collected)
      case Apply(Ident(TermName("after")), List(x)) :: xs =>
        val translated = q"val runsAfter: List[String] = ${x}"
        bodyMacros(c)(xs, isChecker, innerBody, translated :: collected)
      case Apply(Ident(TermName("before")), List(x)) :: xs =>
        val translated = q"override val runsBefore: List[String] = ${x}"
        bodyMacros(c)(xs, isChecker, innerBody, translated :: collected)
      case Nil =>
        collected.reverse
      case x :: xs =>
        bodyMacros(c)(xs, isChecker, innerBody, x :: collected)
    }
  }


  private def generateBody(c: Context)(body: List[c.universe.Tree],
        isChecker: Boolean): List[c.universe.Tree] = {

    import c.universe._
    val kindStr = isChecker match {
      case false => "@phase"
      case true => "@checker" 
    }
    val nameTree: Tree = getNameTree(c, kindStr)

    val (obody, ibody) = splitInnerAndOuterBody(c)(body, isChecker, Nil, Nil)


    val tbody1 = nameTree :: obody

    val runsRightAfter = extractRightAfter(c)(tbody1)




    // If "rightAfter" is declared, "after" won't be necessary
    val hasRunsAfter = doesHaveAfter(c)(tbody1)

    val tbody2: List[c.universe.Tree] = 
      if(runsRightAfter != None && !hasRunsAfter) {
        q"after(List(${runsRightAfter.get}))" :: tbody1
      } else {
        tbody1
      }

      bodyMacros(c)(tbody2, isChecker, ibody, Nil)
  }
}

