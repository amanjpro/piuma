//package ch.usi.inf.l3.lombrello.tquotes
//
//import scala.reflect.macros.runtime.Context
//
//abstract class TQuotes extends Parsers
//                              with Holes
//                              with Placeholders
//                              with Reifiers {
//  val c: Context
//  val global: c.universe.type = c.universe
//  import c.universe._
//  
//  def debug(msg: String): Unit =
//    if (settings.Yquasiquotedebug.value) println(msg)
//
//  lazy val (universe: Tree, args, parts, parse, reify) = c.macroApplication match {
//    case Apply(Select(Select(Apply(Select(universe0, _), List(Apply(_, parts0))), interpolator0), method0), args0) =>
//      val parts1 = parts0.map {
//        case lit @ Literal(Constant(s: String)) => s -> lit.pos
//        case part => c.abort(part.pos, "Quasiquotes can only be used with literal strings")
//      }
//      val reify0 = method0 match {
//        case nme.apply   => new ApplyReifier().reifyFillingHoles(_)
//        case nme.unapply => new UnapplyReifier().reifyFillingHoles(_)
//        case other       => global.abort(s"Unknown quasiquote api method: $other")
//      }
//      
//      val parse0 = interpolator0 match {
//        case e       => TermParser.parse(_)
//        case u      => TypeParser.parse(_)
//        case other       => global.abort(s"Unknown quasiquote flavor: $other")
//      }
//      (universe0, args0, parts1, parse0, reify0)
//    case _ =>
//      global.abort(s"Couldn't parse call prefix tree ${c.macroApplication}.")
//  }
//
//  lazy val ur = universe // shortcut
////  lazy val universeTypes = new definitions.UniverseDependentTypes(universe)
//
//  def expandQuasiquote = {
//    debug(s"\ncode to parse:\n$code\n")
//    val tree = parse(code)
//    debug(s"parsed:\n${showRaw(tree)}\n$tree\n")
//    val reified = reify(tree)
//    debug(s"reified tree:\n$reified\n")
//    reified
//  }
//  
//  
//  val e = newTermName("e")
//  val u = newTermName("u")
//  
//  
////  class UniverseDependentTypes(universe: Tree) {
////      lazy val universeType = universe.tpe
////      lazy val universeSym = universe.symbol
////      lazy val nameType = universeMemberType(tpnme.Name)
////      lazy val termNameType = universeMemberType(tpnme.TypeName)
////      lazy val typeNameType = universeMemberType(tpnme.TermName)
////      lazy val modsType = universeMemberType(tpnme.Modifiers)
////      lazy val flagsType = universeMemberType(tpnme.FlagSet)
////      lazy val symbolType = universeMemberType(tpnme.Symbol)
////      lazy val treeType0 = universeMemberType(tpnme.Tree)
////      lazy val treeType = universeMemberType(tpnme.Tree)
////      lazy val typeDefType = universeMemberType(tpnme.TypeDef)
////      lazy val caseDefType = universeMemberType(tpnme.CaseDef)
////      lazy val iterableTreeType = appliedType(IterableClass, treeType)
////      lazy val iterableCaseDefType = appliedType(IterableClass, caseDefType)
////      lazy val iterableIterableTreeType = appliedType(IterableClass, iterableTreeType)
////      lazy val listTreeType = appliedType(ListClass, treeType)
////      lazy val listListTreeType = appliedType(ListClass, listTreeType)
////      lazy val optionTreeType = appliedType(OptionClass, treeType)
////      lazy val optionNameType = appliedType(OptionClass, nameType)
////      def universeMemberType(name: TypeName) = universe.tpe.memberType(getTypeMember(universe.symbol, name))
////    }
//  }
//}