package ch.usi.inf.l3.lombrello.dsl.symbols

/**
 * @author Amanj Sherwany
 * @date 16 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import scala.annotation.tailrec


trait Symtabs {self: Compiler =>
  class Symtab {
    private var symbols: List[Symbol] = Nil


    def add(sym: Symbol): Unit = {
      symbols.contains(sym) match {
        case true =>
          // TODO: Fix this
          throw new Exception("The symbol is already defined")
        case _ =>
          symbols = sym :: symbols
      }
    }


    def remove(sym: Symbol): Unit = {
      symbols = symbols.filter(_ != sym)
    }

    def contains(sym: Symbol): Boolean = {
      symbols.contains(sym)
    }
  }
}
