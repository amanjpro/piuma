/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin

trait SymbolsUtil { self: LombrelloPlugin =>
  import self.global._

  /**
    * Returns the class symbol that represents an annotation
    *
    * @param qual the fully qualified name of the annotation
    * 
    * @return the class symbol of an annotation that has the full name given
    *         by qual.
    */
  def getAnnotation(qual: String): ClassSymbol = 
    rootMirror.getClassByName(newTypeName(qual))

  /**
    * Returns the info of an annotation that is present on a tree
    *
    * @param tree the tree that might have the annotation
    * @param anno the fully qualified name of the annotation
    * 
    * @return the info of the annotation that has the is present on tree
    */
  def getAnnotationInfo(tree: Tree, anno: String): Option[AnnotationInfo] = {
    val annotation = getAnnotation(anno)
    hasAnnotation(tree, annotation) match {
      case false => None
      case true => 
        val annotationList = tree.symbol.annotations
        // TODO: Make sure that this is the right way
        annotationList.find(p => p.symbol.tpe == annotation.tpe)
    }
  }
  
  /**
    * Returns the info of an annotation that is present on a tree
    *
    * @param symbol the symbol that might have the annotation
    * @param anno the fully qualified name of the annotation
    * 
    * @return the info of the annotation that has the is present on tree
    */
  def getAnnotationInfo(symbol: Symbol, anno: String): Option[AnnotationInfo] = {
    val annotation = getAnnotation(anno)
    hasAnnotation(symbol, annotation) match {
      case false => None
      case true => 
        val annotationList = symbol.annotations
        // TODO: Make sure that this is the right way
        annotationList.find(p => p.symbol.tpe == annotation.tpe)
    }
  }
  /**
    * Checks weather a ValDef is var
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is var, and false otherwise
    */
  def isVar(x: ValDef): Boolean = isVar(x.symbol)

  /**
    * Checks weather a ValDef is val
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is val, and false otherwise
    */
  def isVal(x: ValDef): Boolean = isVal(x.symbol)

  /**
    * Checks weather a ValDef is final
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is final, and false otherwise
    */
  def isFinal(x: Tree): Boolean = isFinal(x.symbol)

  /**
    * Checks weather a ValDef is parameter
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is parameter, and false otherwise
    */
  def isParam(x: Tree): Boolean = isParam(x.symbol)


  /**
    * Checks if a tree has an annotation
    *
    * @param x the tree that might have annotation
    * @param anno the annotaion that might be on x
    *
    * @return true if x has anno, and flase otherwise
    */
  def hasAnnotation(x: Tree, anno: ClassSymbol): Boolean = 
    hasAnnotation(x.symbol, anno)

  /**
    * Checks if a tree has an annotation
    *
    * @param x the tree that might have annotation
    * @param anno the name of then annotaion that might be on x
    *
    * @return true if x has anno, and flase otherwise
    */
  def hasAnnotation(x: Tree, anno: String): Boolean = 
    hasAnnotation(x.symbol, getAnnotation(anno))

  /**
    * Checks if a tree has a good symbol
    * 
    * @param x the tree to be checked
    *
    * @return true, if x has a good symbol, and false otherwise
    *
    * @see goodSymbol(Symbol): Boolean
    */
  def hasSymbol(x: Tree): Boolean = goodSymbol(x.symbol)
  
  /**
    * Checks weather a ValDef is var
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is var, and false otherwise
    */
  def isVar(x: Symbol): Boolean = goodSymbol(x) && x.isVar

  /**
    * Checks weather a ValDef is val
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is val, and false otherwise
    */
  def isVal(x: Symbol): Boolean = goodSymbol(x) && x.isVal

  /**
    * Checks weather a ValDef is final
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is final, and false otherwise
    */

  def isFinal(x: Symbol): Boolean = goodSymbol(x) && x.isFinal

  /**
    * Checks weather a ValDef is parameter
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is parameter, and false otherwise
    */
  def isParam(x: Symbol): Boolean = goodSymbol(x) && x.isValueParameter


  /**
    * Checks if a tree has an annotation
    *
    * @param x the tree that might have annotation
    * @param anno the annotaion that might be on x
    *
    * @return true if x has anno, and flase otherwise
    */
  def hasAnnotation(x: Symbol, anno: ClassSymbol): Boolean = 
    goodSymbol(x) && x.hasAnnotation(anno)

  /**
    * Checks if a tree has an annotation
    *
    * @param x the tree that might have annotation
    * @param anno the name of the annotaion that might be on x
    *
    * @return true if x has anno, and flase otherwise
    */
  def hasAnnotation(x: Symbol, anno: String): Boolean = 
    goodSymbol(x) && x.hasAnnotation(getAnnotation(anno))

  /**
    * Checks if a symbol is not null, neither NoSymbol
    *
    * @param x the symbol to be checked
    *
    * @return true if x is not null, and not NoSymbol. Otherise, false
    */
  def goodSymbol(x: Symbol): Boolean = x != null && x != NoSymbol

  /**
    * Returns a symbol that reprents a class with the given name
    *
    * @param name the fully qualified name of the class
    *
    * @param the symbol of the class that has name
    */
  def getClassByName(name: String): Symbol = 
    rootMirror.getClassByName(newTypeName(name))


}
