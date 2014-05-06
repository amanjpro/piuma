package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import ch.usi.inf.l3.lombrello.dsl
import dsl.source.Position
import dsl.Names

trait Trees {
  
  sealed trait Tree {
    def position: Option[Position]
  }


  case class Program(trees: List[Tree]) extends Tree {
    def position: Option[Position] = None
  }

  case object NoTree extends Tree {
    def position: Option[Position] = None
  }

  case class Comment(mod: CommentMod, verbatim: String, 
    pos: Position) extends PositionedTree 

  sealed trait PositionedTree extends Tree {
    val pos: Position
    def position: Option[Position] = Some(pos)
  }



  // Definitions
  case class PackageDef(pid: SelectOrIdent, trees: List[Tree], 
        pos: Position) extends PositionedTree
  case class DefDef(isPrivate: Boolean, name: Ident, tparams: List[TParamDef],
        params: List[DefDef], tpe: Ident, rhs: Expression, 
        pos: Position) extends PositionedTree 
  case class PluginDef(name: Ident, phases: List[SelectOrIdent], body: List[DefDef], 
        pos: Position) extends PositionedTree
  case class PhaseDef(name: Ident, pluginName: String, preamble: List[Assign], 
        perform: DefDef, body: List[Tree], pos: Position) extends PositionedTree {
    val isChecker: Boolean = perform.name.name == Names.CHECKER
    val isTransformer: Boolean = perform.name.name == Names.TRANSFORMER
    val kind: PhaseKind = if(isChecker) CheckerPhase else TransformerPhase
  }
  case class TParamDef(name: Ident, ubound: Ident, lbound: Ident, pos: Position)
        extends PositionedTree


  // Expressions
  sealed trait Expression extends PositionedTree

  // Block
  // case class ScalaBlock(block: String, pos: Position) extends PositionedTree
  case class Block(stats: List[Tree], expr: Tree, 
    pos: Position) extends Expression   

  // Function abstraction and application
  case class Function(params: List[DefDef], rhs: Expression, pos: Position)
      extends Expression
  case class Apply(method: SelectOrIdent, args: List[Expression], 
      pos: Position) extends Expression
  case class Literal(value: Any, pos: Position) extends Expression {
    val valueAsString = value.toString
  }
  
  case object EmptyExpression extends Expression {
    val pos = Position()
  }
  // Select or Ident
  sealed trait SelectOrIdent extends PositionedTree

  case class Select(qual: SelectOrIdent, id: Ident, pos: Position) 
      extends Expression with SelectOrIdent
  case class Ident(name: String, pos: Position) extends Expression
    with SelectOrIdent


  // Branching
  case class Match(cond: Expression, cases: List[CaseDef], pos: Position) 
      extends Expression 

  // TODO: Do a better thing for pattern field in CaseDef
  case class CaseDef(pattern: Tree, rhs: Expression, 
    pos: Position) extends PositionedTree 

  case class If(cond: Expression, thenp: Expression, elsep: Expression, 
    pos: Position) extends Expression 
  


  // Binary and Unary operators
  case class Binary(lhs: Expression, op: BinOps, rhs: Expression, 
    pos: Position) extends Expression
  case class Unary(op: UniOps, operand: Expression, 
    pos: Position) extends Expression


  // Misc
  case class Import(id: SelectOrIdent, pos: Position) extends PositionedTree
  case class Assign(lhs: SelectOrIdent, rhs: Expression, 
    pos: Position) extends PositionedTree
  case class PropertyTree(property: PropertyType, value: String, pos: Position) 
      extends PositionedTree


  sealed trait PropertyType

  case object RunsAfterProperty extends PropertyType
  case object RunsRightAfterProperty extends PropertyType
  case object RunsBeforeProperty extends PropertyType


  sealed trait UniOps
  case object Negative extends UniOps
  case object Not extends UniOps

  sealed trait BinOps
  case object Add extends BinOps
  case object Sub extends BinOps
  case object Mul extends BinOps
  case object Div extends BinOps
  case object Mod extends BinOps
  case object And extends BinOps
  case object Or extends BinOps
  case object XOR extends BinOps
  case object LT extends BinOps
  case object GT extends BinOps
  case object LE extends BinOps
  case object GE extends BinOps
  case object Eq extends BinOps
  case object Neq extends BinOps

  sealed trait CommentMod
  case object LineComment extends CommentMod
  case object BlockComment extends CommentMod

  sealed trait PhaseKind
  case object CheckerPhase extends PhaseKind
  case object TransformerPhase extends PhaseKind
}
