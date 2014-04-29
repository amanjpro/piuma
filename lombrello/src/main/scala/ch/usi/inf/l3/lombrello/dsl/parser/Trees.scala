package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

trait Trees {
  
  sealed abstract class Tree {
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

  sealed abstract class PositionedTree extends Tree {
    val pos: Position
    def position: Option[Position] = Some(pos)
  }



  // Definitions
  case class DefDef(name: Ident, params: List[DefDef], tpe: TypeTree, 
        rhs: Expression, pos: Position) extends PositionedTree 


  // Block
  case class ScalaBlock(block: String, pos: Position) extends PositionedTree
  case class Block(stats: List[Tree], expr: Tree, 
    pos: Position) extends PositionedTree   
  // Expressions
  sealed abstract class Expression extends PositionedTree
  case class Apply(method: SelectOrIdent, args: List[Expression], 
      pos: Position) extends Expression
  case class Literal[T <: AnyVal](value: T, pos: Position) extends Expression 
  
  // Select or Ident
  sealed abstract class SelectOrIdent extends Expression
  case class Select(quals: SelectOrIdent, id: Ident, pos: Position) 
      extends SelectOrIdent   
  case class Ident(name: String, pos: Position) extends SelectOrIdent
  case class WildCard(pos: Position) extends SelectOrIdent


  // Branching
  case class Match(cond: Expression, cases: List[CaseDef], pos: Position) 
      extends Expression 

  // TODO: Do a better thing for pattern field in CaseDef
  case class CaseDef(pattern: Tree, rhs: Block, 
    pos: Position) extends PositionedTree 

  case class If(cond: Expression, thenp: Block, elsep: Block, pos: Position)
      extends Expression 
  


  // Binary and Unary operators
  case class Binary(lhs: Expression, op: BinOps, rhs: Expression, 
    pos: Position) extends Expression
  case class Unary(op: UniOps, operand: Expression, 
    pos: Position) extends Expression


  // Misc
  case class Import(id: SelectOrIdent, pos: Position) extends PositionedTree
  case class Assign(lhs: SelectOrIdent, rhs: Expression, 
    pos: Position) extends PositionedTree
  case class TypeTree(id: SelectOrIdent, pos: Position) extends PositionedTree
  case class PropertyTree(property: PropertyType, value: String, pos: Position) 
      extends PositionedTree


  sealed abstract class PropertyType

  case object NameProperty extends PropertyType
  case object RunsAfterProperty extends PropertyType
  case object RunsRightAfterProperty extends PropertyType
  case object RunsBeforeProperty extends PropertyType


  sealed abstract class UniOps
  case object Negative extends UniOps
  case object Not extends UniOps

  sealed abstract class BinOps
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

  sealed abstract class CommentMod
  case object LineComment extends CommentMod
  case object BlockComment extends CommentMod
}







