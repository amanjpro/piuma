package ch.usi.inf.l3.lombrello.neve


import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation


object NeveDSL {
  class phase extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro phaseMacro.impl
  }


  object phaseMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          println(clazz.impl.parents)
          clazz
        case x => 
          c.abort(c.enclosingPosition, 
            "@phase can only be applied on classes")
          EmptyTree
      }


      c.Expr[Any](expandee)
    }
  }


  class plugin extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro pluginMacro.impl
  }

  object pluginMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val inputs = annottees.map(_.tree).toList
      val expandee = inputs match {
        case (clazz: ClassDef) :: Nil => 
          println(clazz.impl.parents)
          clazz
        case x => 
          c.abort(c.enclosingPosition, 
            "@plugin can only be applied on classes")
          EmptyTree
      }


    c.Expr[Any](expandee)
    }
  }
}

