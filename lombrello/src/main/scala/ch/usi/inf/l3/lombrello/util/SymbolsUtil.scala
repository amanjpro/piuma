package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPlugin

trait SymbolsUtil { self: TransformerPlugin =>
  import self.global._
  def getAnnotation(qual: String) = rootMirror.getClassByName(newTypeName(qual))
  
  
  def isVar(x: ValDef): Boolean = isVar(x.symbol)
  def isVal(x: ValDef): Boolean = isVal(x.symbol)
  def hasAnnotation(x: Tree, anno: ClassSymbol): Boolean = hasAnnotation(x.symbol, anno)
  def hasSymbol(x: Tree): Boolean = goodSymbol(x.symbol)
  def isFinal(x: Tree): Boolean = isFinal(x.symbol)
  def isParam(x: Tree): Boolean = isParam(x.symbol)
  
  def isFinal(x: Symbol): Boolean = goodSymbol(x) && x.isFinal
  def isVar(x: Symbol): Boolean = goodSymbol(x) && x.isVar
  def isVal(x: Symbol): Boolean = goodSymbol(x) && x.isVal
  def hasAnnotation(x: Symbol, anno: ClassSymbol): Boolean = goodSymbol(x) && x.hasAnnotation(anno)
  def goodSymbol(x: Symbol): Boolean = x != null && x != NoSymbol
  def isParam(x: Symbol): Boolean = goodSymbol(x) && x.isValueParameter
}