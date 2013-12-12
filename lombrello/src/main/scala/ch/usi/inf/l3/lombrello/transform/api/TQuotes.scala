//package ch.usi.inf.l3.lombrello.transform.api
//
//import language.experimental.macros
//import scala.reflect.macros.Context
//import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent
////import scala.tools.nsc.Global
//
//
//
///**
// * Limitations of Quasiquotes over TQuotes:
// * 
// * 1- You can not use quasiquotes with compiler plugins, as they depend on 
// *    Macros, and macros should be statically ready to be used, and you cannot
// *    make everything avaialble during compilation time in plugins!!
// */
//trait TQuotes { self: TransformerPluginComponent =>
//    
//  import self.global._
//  
//  //  self: Global =>
//
//  //  import self._
//
//  object TQuote {
//    def uImpl(c: Context)(ctx: c.Expr[StringContext], 
//        args: c.Expr[Tree]*): c.Expr[Tree] = {
//      val r = merge(c)(ctx.splice.parts.toList, args.toList)
//      null
//    }
//    
//    def eImpl(c: Context)(ctx: c.Expr[StringContext],
//        subpatterns: c.Expr[Tree]*): c.Expr[Option[Tree]] = {
////      c.Expr(Some(subpatterns.mkString))
//      null
//    }
//    
//    def merge(c: Context)(parts: List[String], args: List[c.Expr[Tree]]): String = {
//      (parts, args) match {
//        case (Nil, Nil) => ""
//        case (p :: ps, Nil) => p + merge(c)(ps, Nil)
//        case (Nil, a :: as) => a + merge(c)(Nil, as)
//        case (p :: ps, a :: as) => p + a + merge(c)(ps, as)
//      }
//    }
//  }
//  implicit class TQuote(ctx: StringContext) {
//    object u {
//      def apply(args: Tree*): Tree = macro ???
//    }
//
//    object e {
//      def unapply(subpatterns: Tree*): Option[Any] = macro ???
//    }
//
//    
//
////    def parse(str: String): Tree = {
////
////    }
//
////        def translate(src: String): self.Tree = {
////          src match {
////            case "val" => ValDef
////          }
////        }
//  }
//
//  //  
//  //    
//  //  def u(args: Any*): Any = u(args)
//  //    
//  //  def e(subpatterns: Any*): Option[Any] = e.unapply(subpatterns)
//}
//
