/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.piuma.util

import ch.usi.inf.l3.piuma.plugin.PiumaPlugin

trait ScopeCake { self: PiumaPlugin =>

  type SSymbol = self.global.Symbol

  abstract class Scope[T] {
    /** Enter a nested scope */
    def enterScope: Scope[T]
    /** Exit the current scope and return the outer scope */
    def exitScope: Scope[T]
    /** Add the given item to the scope */
    def decl(sym: SSymbol, item: T): Scope[T]
    /** Remove the given item from the scope */
    def del(sym: SSymbol): Scope[T]
    /** Find declaration in this or outer scopes */
    def value(sym: SSymbol): T
    /** Return a set of all declared symbols */
    def decls: Set[SSymbol]
    def exist(sym: SSymbol): Boolean
    
    def decl(l: List[(SSymbol, T)]): Scope[T] = {
      l match {
        case Nil => this
        case (x1, x2) :: xs => decl(x1, x2).decl(xs)
      }
    }
    def del(l: List[SSymbol]): Scope[T] = {
      l match {
        case Nil => this
        case x :: xs => del(x).del(xs)
      }
    }
  }
}
