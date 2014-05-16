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
    def tpe: self.Type
    def declarations: List[Symbol]
    def has(sym: Symbol): Boolean
    def directlyHas(sym: Symbol): Boolean
    def add(sym: Symbol): Boolean
    def remove(sym: Symbol): Boolean
  }


  case class TypeSymbol(tpe: self.NeveType) extends Symbol {
    // TODO: Fix this
    private var owner: Symbol = NoSymbol
    private var decls: List[Symbol] = Nil
    def declarations: List[Symbol] = decls
    def has(sym: Symbol): Boolean = decls.contains(sym) || owner.has(sym)
    def directlyHas(sym: Symbol): Boolean = decls.contains(sym)
    def add(sym: Symbol): Boolean = {
      if(directlyHas(sym)) {
        false
      } else {
        decls = sym :: decls
        true
      }
    }
    def remove(sym: Symbol): Boolean = {
      if(directlyHas(sym)) {
        decls = decls.filter(_ != sym)
        true
      } else {
        false
      }
    }
  }

  case class ScalaSymbol(tpe: self.ScalaType) extends Symbol {
    def declarations: List[Symbol] = ???
    def has(sym: Symbol): Boolean  = ???
    def directlyHas(sym: Symbol): Boolean = ???
    def add(sym: Symbol): Boolean = ???
    def remove(sym: Symbol): Boolean = ???
  }

  case class TermSymbol(tpe: self.TermType) extends Symbol {
    def declarations: List[Symbol] = ???
    def has(sym: Symbol): Boolean  = ???
    def directlyHas(sym: Symbol): Boolean = ???
    def add(sym: Symbol): Boolean = ???
    def remove(sym: Symbol): Boolean = ???
  }

  case object NoSymbol extends Symbol {
    val tpe: self.Type = NoType
    def declarations: List[Symbol] = ???
    def has(sym: Symbol): Boolean  = ???
    def directlyHas(sym: Symbol): Boolean = ???
    def add(sym: Symbol): Boolean = ???
    def remove(sym: Symbol): Boolean = ???
  }

}
