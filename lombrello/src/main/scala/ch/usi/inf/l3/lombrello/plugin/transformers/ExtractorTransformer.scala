/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags
import scala.annotation.tailrec

/**
  * This trait, enables extracting various kinds of trees.
  */
trait ExtractorTransformerCake {
  extractor: TransformerPluginComponent =>
  trait ExtractorTransformer {
    self: extractor.TransformerComponent =>

    import extractor.plgn._
    import extractor.plgn.global._


    def findFreeVars(stmt: Tree,
                              captured: List[Symbol],
                              acc: List[Symbol]): List[Symbol] = {
      // TODO: Implement this
      acc
    }

    @tailrec final def findFreeVars(stmts: List[Tree],
                             captured: List[Symbol],
                             acc: List[Symbol]): List[Symbol] = {
      stmts match {
        case (x: ValDef) :: xs if goodSymbol(x.symbol) =>
          val frees = findFreeVars(x, captured, acc)
          findFreeVars(xs, x.symbol :: captured, acc ++ frees)
        case x :: xs =>
          val frees = findFreeVars(x, captured, acc)
          findFreeVars(xs, captured, acc ++ frees)
        case Nil =>
          acc
      }
    }


    /**
      * Extracts a method out of a list of statements, and replaces them
      * with a call to the method.
      *
      * @param stmts the list of statements to be extracted to a method,
      *        and substituted with a method call
      * @param owner the owner of the extracted method
      * @param methodName the name of the extracted method
      *
      * @return Some tuple of the extracted method and a call to that method,
      *         or None if the list is empty.
      */
    def extractMethod(stmts: List[Tree],
                      owner: Symbol, 
                      methodName: TermName): Option[(DefDef, Option[Apply])] = {
      stmts match {
        case Nil => None
        case xs =>
          // TODO: set proper flags
          val flags = 0
          val msymbol = owner.newMethodSymbol(
            methodName, owner.pos.focus, flags)
          val freeVars = findFreeVars(stmts, Nil, Nil)
          // TODO: fix the owner
          // fixOwner()
          //TODO: Fix the rhs
          val rhs = Block(stmts.dropRight(1), stmts.last)

          //TODO: Fix params and tparams
          val tparams: List[TypeDef] = Nil
          val params: List[ValDef] = Nil
          val tpt = EmptyTree
          val mthd = DefDef(Modifiers(flags), methodName, tparams, 
            List(params), tpt, rhs).setSymbol(msymbol)
          
          localTyper.typed { mthd }.asInstanceOf[DefDef]

          None
      }
    }
  }
}
