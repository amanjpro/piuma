/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.util

import ch.usi.inf.l3.lombrello.plugin.LombrelloPlugin

trait SymbolsUtil { self: LombrelloPlugin =>
  import self.global._

  // TODO: CHECK AGAINST SYMBOL NULL or NOSYMBOL throughout this class

  //TODO: You should re-locate this method, it has nothing to do 
  // with Symbol
  /**
    * Returns a list of applied arguments of an annotation.
    *
    * @param symbol the symbol that has annotation
    * @param qual the fully qualified name of the intended annotation
    *
    * @return a list of applied arguments of an annotation.
    */
  def getAnnotationArguments(symbol: Symbol, qual: String): List[Tree] = {
    val annoInfo = getAnnotationInfo(symbol, qual)
    getAnnotationArguments(annoInfo)
  }


  /**
    * Returns the symbol of the class that owns a tree. If the tree is not 
    * owned by a class, then it returns None.
    *
    * Top level trees can be owned by no classes, like Packages, Classes,
    * Modules (Objects).
    *
    * @param tree the tree that might be owned by a class
    *
    * @return Some symbol of the class that owns the given tree, or None.
    */
  def getOwnerClass(tree: Tree): Option[ClassSymbol] = {
    getOwnerClass(tree.symbol)
  }

  /**
    * Returns the symbol of the class that owns a tree direcctly.
    * If the tree is not directly owned by a class, then it returns
    * None.
    *
    * @param tree the tree that might be owned by a class
    *
    * @return Some symbol of the class that owns the given tree 
    *          directly, or None.
    */
  def getOwnerIfClass(tree: Tree): Option[ClassSymbol] = {
    getOwnerIfClass(tree.symbol)
  }


  /**
    * Returns a list of applied arguments of an annotation.
    *
    * @param tree the tree that has annotation
    * @param qual the fully qualified name of the intended annotation
    *
    * @return a list of applied arguments of an annotation.
    */
  def getAnnotationArguments(tree: Tree, qual: String): List[Tree] = {
    getAnnotationArguments(tree.symbol, qual)
  }

  /**
    * Returns a list of applied arguments of an annotation.
    *
    * @param annoInfo the optional annotation info of an annotation symbol
    *
    * @return a list of applied arguments of an annotation, if annoInfo is None
    *         then return Nil.
    */
  def getAnnotationArguments(annoInfo: Option[AnnotationInfo]): List[Tree] = {
    annoInfo match {
      case None => Nil
      case Some(info) => 
        info.args
    }
  }

  /**
    * Returns a list of applied arguments of an annotation.
    *
    * @param annoInfo the annotation info of an annotation symbol
    *
    * @return a list of applied arguments of an annotation
    */
  def getAnnotationArguments(annoInfo: AnnotationInfo): List[Tree] = {
    annoInfo.args
  }

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
  def isVar(x: Tree): Boolean = isVar(x.symbol)

  /**
    * Checks weather a ValDef is val
    *
    * @param x the tree that represents ValDef
    *
    * @return true if x is val, and false otherwise
    */
  def isVal(x: Tree): Boolean = isVal(x.symbol)

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
    * Checks weather a tree has a symbol that represents an abstract member
    *
    * @param x the tree to be checked
    *
    * @return true if x represents an abstract member, and false otherwise
    */
  def isAbstract(x: Tree): Boolean = isAbstract(x.symbol)

  /**
    * Checks weather a tree has a symbol that represents a public member
    *
    * @param x the tree to be checked
    *
    * @return true if x represents a public member, and false otherwise
    */
  def isPublic(x: Tree): Boolean = isPublic(x.symbol)


  /**
    * Checks weather a tree is pacakge
    *
    * @param x the tree to be checked
    *
    * @return true if x is package, and false otherwise
    */
  def isPackage(x: Tree): Boolean = isPackage(x.symbol)

  /**
    * Checks weather a Tree represents a method 
    *
    * @param x the tree to be checked
    *
    * @return true if x is method, and false otherwise
    */
  def isMethod(x: Tree): Boolean = isMethod(x.symbol)

  /**
    * Checks weather a Tree is constructor
    *
    * @param x the tree that represents a method
    *
    * @return true if x is constructor, and false otherwise
    */
  def isConstructor(x: Tree): Boolean = isConstructor(x.symbol)

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
    * @param x the symbol that represents ValDef
    *
    * @return true if x is var, and false otherwise
    */
  def isVar(x: Symbol): Boolean = goodSymbol(x) && x.isVar

  /**
    * Checks weather a ValDef is val
    *
    * @param x the symbol that represents ValDef
    *
    * @return true if x is val, and false otherwise
    */
  def isVal(x: Symbol): Boolean = goodSymbol(x) && x.isVal

  /**
    * Checks weather a ValDef is final
    *
    * @param x the symbol that represents ValDef
    *
    * @return true if x is final, and false otherwise
    */

  def isFinal(x: Symbol): Boolean = goodSymbol(x) && x.isFinal

  /**
    * Checks weather a ValDef is parameter
    *
    * @param x the symbol that represents ValDef
    *
    * @return true if x is parameter, and false otherwise
    */
  def isParam(x: Symbol): Boolean = goodSymbol(x) && x.isValueParameter

  /**
    * Checks weather a symbol represents an abstract member
    *
    * @param x the symbol to be checked
    *
    * @return true if x represents an abstract member, and false otherwise
    */
  def isAbstract(x: Symbol): Boolean = goodSymbol(x) && x.isDeferred

  /**
    * Checks weather a symbol represents a public member
    *
    * @param x the symbol to be checked
    *
    * @return true if x represents a public member, and false otherwise
    */
  def isPublic(x: Symbol): Boolean = goodSymbol(x) && x.isPublic


  /**
    * Checks weather a symbol represents a package
    *
    * @param x the symbol to be checked
    *
    * @return true if x represents a package, and false otherwise
    */
  def isPackage(x: Symbol): Boolean = goodSymbol(x) && x.hasPackageFlag

  /**
    * Checks weather a Symbol represents a method
    *
    * @param x the symbol to be checked
    *
    * @return true if x is method, and false otherwise
    */
  def isMethod(x: Symbol): Boolean = goodSymbol(x) && x.isMethod

  /**
    * Checks weather a Symbol is constructor
    *
    * @param x the symbol that represents a method
    *
    * @return true if x is constructor, and false otherwise
    */
  def isConstructor(x: Symbol): Boolean = goodSymbol(x) && x.isConstructor


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


  /**
    * Returns the symbol of the class that owns a symbol. If the symbol is not 
    * owned by a class, then it returns None.
    *
    * Top level symbols can be owned by no classes, like Packages, Classes,
    * Modules (Objects).
    *
    * @param symbol the symbol that might be owned by a class
    *
    * @return Some symbol of the class that owns the given symbol, or None.
    */
  def getOwnerClass(symbol: Symbol): Option[ClassSymbol] = {
    if(goodSymbol(symbol)) { //TODO: Is this enough?
      symbol.owner match {
        case s: ClassSymbol => Some(s)
        case s: MethodSymbol if (s.isClassConstructor) => Some(s.owner.asClass)
        case owner => getOwnerClass(owner)
      }
    } else {
      None
    }
  }

  /**
    * Returns the symbol of the class that owns a symbol direcctly.
    * If the symbol is not directly owned by a class, then it returns
    * None.
    *
    * @param symbol the symbol that might be owned by a class
    *
    * @return Some symbol of the class that owns the given symbol 
    *          directly, or None.
    */
  def getOwnerIfClass(symbol: Symbol): Option[ClassSymbol] = {
    if(goodSymbol(symbol)) {
      symbol.owner match {
        case s: ClassSymbol => Some(s)
        case s: MethodSymbol if (s.isClassConstructor) => Some(s.owner.asClass)
        case _ => None
      }
    } else {
      None
    }
  }
}
