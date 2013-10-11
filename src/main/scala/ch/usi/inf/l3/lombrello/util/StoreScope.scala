package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin

abstract class StoreScope[T] extends Scope[T] { self: TransformerPlugin =>
  private val env = Map.empty[SSymbol, T]
}