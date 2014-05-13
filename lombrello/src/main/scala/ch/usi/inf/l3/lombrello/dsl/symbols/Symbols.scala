package ch.usi.inf.l3.lombrello.dsl.symbols

/**
 * @author Amanj Sherwany
 * @date 13 May 2014
 */

import ch.usi.inf.l3.lombrello.dsl
import dsl.source.Position
import dsl.Names
import dsl.typechecker._


trait Symbols { self: dsl.Compiler =>
  
  sealed trait Symbol {
    val tpe: self.Type
  }

  case object NoSymbol extends Symbol {
    val tpe: self.Type = NoType
  }

}
