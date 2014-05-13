package ch.usi.inf.l3.lombrello.dsl.typechecker

/**
 * @author Amanj Sherwany
 * @date 13 May 2014
 */

import ch.usi.inf.l3.lombrello.dsl
import dsl.source.Position
import dsl.Names


trait Types { self: dsl.Compiler => 
  sealed trait Type

  case class NeveType(name: String, members: List[self.Symbol]) extends Type

  case class ScalaType(name: String) extends Type

  case class MethodType(name: String, params: List[self.Symbol], 
        ret: self.Symbol) extends Type

  case object NoType extends Type

}
