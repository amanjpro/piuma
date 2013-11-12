/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin

abstract class StoreScope[T] extends Scope[T] { self: TransformerPlugin =>
  private val env = Map.empty[SSymbol, T]
}