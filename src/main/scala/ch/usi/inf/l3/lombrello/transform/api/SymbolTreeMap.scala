/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.transform.api

import scala.tools.nsc.Global

private[transform] trait SymbolTreeMap {

  val global: Global
  
  import global._
  
  private var bank = Map.empty[Symbol, Tree]
  
  
  def add(sym: Symbol, tree: Tree) = bank = bank + (sym -> tree)
  
  def getOption(sym: Symbol): Option[Tree] = bank.get(sym)
  
  def get(sym: Symbol) = bank(sym)
}