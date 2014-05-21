package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import ch.usi.inf.l3.lombrello.dsl
import dsl.source.Position
import dsl.Names
import dsl.symbols
import scala.annotation.tailrec

trait Trees { self: dsl.Compiler =>
  
  sealed trait Tree {
    def position: Option[Position]
    def symbol: Symbol
  }


  case class Program(trees: List[PackageDef]) extends Tree {
    def position: Option[Position] = None
    def symbol: Symbol = NoSymbol
  }

  case object NoTree extends Tree {
    def position: Option[Position] = None
    def symbol: Symbol = NoSymbol
  }

  case class Comment(mod: CommentMod, verbatim: String, 
    pos: Position) extends PositionedTree {
    def symbol: Symbol = NoSymbol
  }

  sealed trait PositionedTree extends Tree {
    val pos: Position
    def position: Option[Position] = Some(pos)
  }



  // Definitions
  case class PackageDef(pid: SelectOrIdent, trees: List[Tree], 
        pos: Position, symbol: Symbol = NoSymbol) extends PositionedTree {
    lazy val pidString: String = pid.asString   
  }

  case class DefDef(mod: Modifier, name: Ident, tparams: List[TParamDef],
        params: List[DefDef], tpe: TypeTree, rhs: Expression, 
        pos: Position, symbol: Symbol = NoSymbol) extends PositionedTree 
  case class PluginDef(name: Ident, phases: List[SelectOrIdent], body: List[DefDef], 
      pos: Position, symbol: Symbol = NoSymbol) extends PositionedTree
  case class PhaseDef(name: Ident, phaseName: String, 
    preamble: List[PropertyTree], perform: DefDef, body: List[Tree], 
        pos: Position, symbol: Symbol = NoSymbol) extends PositionedTree {
    val isChecker: Boolean = perform.name.name == Names.CHECKER
    val isTransformer: Boolean = perform.name.name == Names.TRANSFORMER
    val kind: PhaseKind = if(isChecker) CheckerPhase else TransformerPhase
  }

  case class TParamDef(name: Ident, lbound: TypeTree, ubound: TypeTree, 
    pos: Position, symbol: Symbol = NoSymbol) extends PositionedTree

  sealed trait TypeTree extends PositionedTree
  case class SimpleTypeTree(id: SelectOrIdent, tparams: List[TypeTree], 
        pos: Position, symbol: Symbol = NoSymbol) extends TypeTree
  case class ProductTypeTree(items: List[TypeTree], 
        pos: Position, symbol: Symbol = NoSymbol) extends TypeTree
  case class FunctionTypeTree(params: List[TypeTree], ret: TypeTree, 
        pos: Position, symbol: Symbol = NoSymbol) extends TypeTree

  // Expressions
  sealed trait Expression extends PositionedTree

  // Block
  // case class ScalaBlock(block: String, pos: Position) extends PositionedTree
  case class Block(stats: List[Tree], expr: Tree, 
    pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }   

  // Function abstraction and application
  case class Function(params: List[DefDef], rhs: Expression, pos: Position)
      extends Expression {
    def symbol: Symbol = NoSymbol
  }
  case class Apply(method: Expression, targs: List[TypeTree], args: List[Expression], 
    pos: Position, symbol: Symbol = NoSymbol) extends Expression

  case class Literal(value: Any, pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
    val valueAsString = value.toString
    val litRep = value match {
      case x: String => "\"" + x + "\""
      case x: Char => s"'${x}'"
      case x => x.toString
    }
  }
  
  // Select or Ident
  sealed trait SelectOrIdent extends Expression {
    def asString: String
  }

  case class Select(qual: Expression, id: IdentOrThis, pos: Position, 
      symbol: Symbol = NoSymbol) extends SelectOrIdent {
   lazy val asString: String = {
      @tailrec def helper(tree: Expression, computed: List[String]): String = {
        tree match {
          case Select(expr, id: Ident, _, _) => helper(expr, id.name :: computed)
          case Ident(name, _, _) => (name :: computed).mkString(".")
          case _ => computed.mkString(".")
        }
      }
      id match {
        case Ident(name, _, _) => s"${helper(qual, Nil)}.${name}"
        case _ => s"${helper(qual, Nil)}.${this}"
      }
    }
  }
  case class Ident(name: String, pos: Position, 
      symbol: Symbol = NoSymbol) extends SelectOrIdent with IdentOrThis {
    lazy val asString: String = name
  }


  sealed trait IdentOrThis extends Expression
  case class This(pos: Position, symbol: Symbol = NoSymbol) extends IdentOrThis

  case class Super(pos: Position, symbol: Symbol = NoSymbol) extends Expression

  // Branching
  case class Match(cond: Expression, cases: List[CaseDef], pos: Position)
      extends Expression {
    def symbol: Symbol = NoSymbol
  }

  case class CaseDef(pattern: Pattern, cond: Option[Expression], 
    rhs: Expression, pos: Position) extends PositionedTree {
    def symbol: Symbol = NoSymbol
  }

  sealed trait Pattern extends PositionedTree
  case class Bind(id: Ident, tpe: SelectOrIdent, 
      pattern: List[Pattern], pos: Position, symbol: Symbol = NoSymbol) extends Pattern
  case class LiteralPattern(l: Literal, pos: Position) extends Pattern {
    def symbol: Symbol = NoSymbol
  }


  case class If(cond: Expression, thenp: Expression, elsep: Expression, 
    pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }
  

  // Try-catch block
  case class Try(cond: Expression, catches: List[CaseDef], 
    fnly: Option[Expression], pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }

  // Binary and Unary operators
  case class Binary(lhs: Expression, op: BinOp, rhs: Expression, 
    pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }
  case class Unary(op: UniOp, operand: Expression, 
    pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }
  case class Record(values: List[Tree], pos: Position) extends Expression {
    def symbol: Symbol = NoSymbol
  }


  // Misc
  case class Import(id: SelectOrIdent, pos: Position) 
      extends PositionedTree {
    def symbol: Symbol = NoSymbol
    lazy val idString: String = id.asString
  }

  case class PropertyTree(property: PropertyType, value: Expression, 
    pos: Position) extends PositionedTree {
    def symbol: Symbol = NoSymbol
  }

  case class New(tpe: SimpleTypeTree, args: List[Expression], 
      pos: Position, symbol: Symbol = NoSymbol) extends Expression {
  }

  case class Throw(exp: Expression, pos: Position, 
        symbol: Symbol = NoSymbol) extends Expression

  sealed trait PropertyType

  case object RunsAfterProperty extends PropertyType
  case object RunsRightAfterProperty extends PropertyType
  case object RunsBeforeProperty extends PropertyType
  case object NoProperty extends PropertyType


  sealed trait UniOp
  case object Negative extends UniOp {
    override def toString: String = "-"
  }
  case object Not extends UniOp {
    override def toString: String = "!"
  }

  sealed trait BinOp
  case object Add extends BinOp {
    override def toString: String = "+"
  }

  case object Sub extends BinOp {
    override def toString: String = "-"
  }

  case object Mul extends BinOp {
    override def toString: String = "*"
  }

  case object Div extends BinOp {
    override def toString: String = "/"
  }

  case object Mod extends BinOp {
    override def toString: String = "%"
  }

  case object And extends BinOp {
    override def toString: String = "&&"
  }

  case object Or extends BinOp {
    override def toString: String = "||"
  }

  case object Pipe extends BinOp {
    override def toString: String = "|"
  }
  case object XOR extends BinOp {
    override def toString: String = "^"
  }

  case object LT extends BinOp {
    override def toString: String = "<"
  }

  case object GT extends BinOp {
    override def toString: String = ">"
  }

  case object LE extends BinOp {
    override def toString: String = "<="
  }
  case object GE extends BinOp {
    override def toString: String = ">="
  }
  case object Eq extends BinOp {
    override def toString: String = "=="
  }
  case object Neq extends BinOp {
    override def toString: String = "!="
  }
  case object Join extends BinOp {
    override def toString: String = "++"
  }
  case object Cons extends BinOp {
    override def toString: String = "::"
  }
  case object To extends BinOp {
    override def toString: String = "->"
  }
  case object SHR extends BinOp {
    override def toString: String = ">>"
  }
  case object SHL extends BinOp {
    override def toString: String = "<<"
  }
  
  sealed trait CommentMod
  case object LineComment extends CommentMod
  case object BlockComment extends CommentMod

  sealed trait PhaseKind
  case object CheckerPhase extends PhaseKind
  case object TransformerPhase extends PhaseKind
}
