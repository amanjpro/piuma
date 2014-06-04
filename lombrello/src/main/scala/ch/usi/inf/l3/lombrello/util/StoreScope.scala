/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin

trait StoreScopeCake extends ScopeCake { self: LombrelloPlugin =>

  class StoreScope[T] private (private val locations: Map[SSymbol, Int] = Map.empty[SSymbol, Int],
    private val values: Map[Int, T] = Map.empty[Int, T],
    private val nextMemoryLocation: Int = 0,
    private val outer: Option[Scope[T]] = None) extends Scope[T] {

    /** Enter a nested scope */
    def enterScope: Scope[T] = {
      new StoreScope(Map.empty, Map.empty, 0, Some(this))
    }
    
    def exist(sym: SSymbol): Boolean = {
      locations.contains(sym)
    }
    /** Exit the current scope and return the outer scope */
    def exitScope: Scope[T] = {
      outer match {
        case Some(scope) => scope
        case _ => throw new Exception("No outer scope exception")
      }
    }
    /** Add the given item to the scope */
    def decl(sym: SSymbol, item: T): Scope[T] = {
      locations.get(sym) match {
        case None =>
          new StoreScope[T](locations + (sym -> nextMemoryLocation),
            values + (nextMemoryLocation -> item), nextMemoryLocation + 1, outer)
        case Some(x) => throw new Exception("Symbols is already defined")
      }
    }
    /** Return a set of all declared symbols */
    def decls: Set[SSymbol] = {
      locations.keySet
    }
    /** Find declaration in this or outer scopes */
    def value(sym: SSymbol): T = {
      locations.get(sym) match {
        case None =>
          outer match {
            case None => throw new Exception("Symbol is not defined")
            case Some(o) => o.value(sym)
          }
        case Some(n) => values(n)
      }
    }
    /** Remove the given item from the scope */
    def del(sym: SSymbol): Scope[T] = {
      locations.get(sym) match {
        case Some(l) => 
          /* To support aliases easily, we never delete values. 
           * True this leads to higher memory consumption, but also
           * to a safer code.
           */
          new StoreScope[T](locations - sym, values, nextMemoryLocation, outer)
        case None => throw new Exception("Symbol not found exception")
      }
    }
  }

  object StoreScope {
    def apply[T](): Scope[T] = {
      new StoreScope[T]()
    }

    def newScope[T]: Scope[T] = {
      apply[T]()
    }
  }
}
