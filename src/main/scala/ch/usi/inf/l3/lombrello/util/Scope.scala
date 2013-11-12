/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin

trait Scope[T] { self: TransformerPlugin =>
  
  type SSymbol = self.global.Symbol
  
  self.global.Scope
  /** Enter a nested scope */
  def nestedScope: Scope[T]
  /** Exit the current scope and return the outer scope */
  def exitScope: Scope[T]
  /** Add the given item to the scope */
  def add(sym: SSymbol, item: T): Scope[T]
  /** Remove the given item from the scope */
  def remove(sym: SSymbol, item: T): Scope[T]
  def addBatch(l: List[(SSymbol, T)]): Scope[T]
  def removeBatch(l: List[SSymbol]): Scope[T]
}