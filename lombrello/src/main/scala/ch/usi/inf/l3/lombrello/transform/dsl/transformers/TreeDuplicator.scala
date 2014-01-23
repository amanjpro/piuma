/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.transform.dsl.transformers

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent
import scala.reflect.internal.Flags
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

trait TreeDuplicatorCake {

  duplicator: TransformerPluginComponent =>

  import duplicator.plugin._
  trait TreeDuplicator {
    self: duplicator.TransformerComponent =>

    import duplicator.plugin.global._

    def duplicate(x: Tree, newName: TermName): Tree = {
      duplicate(x, newName, x.symbol.owner)
    }
    def duplicate(x: Tree, newName: TermName, newOwner: Symbol): Tree = {
      x match {
        case ValDef(_, _, _, _) |
          DefDef(_, _, _, _, _, _) |
          ModuleDef(_, _, _) |
          ClassDef(_, _, _, _) =>
          val newTree = x.duplicate
          val newSym = x.symbol.cloneSymbol(newOwner).setName(newName)
          newSym.setInfoAndEnter(x.symbol.tpe)
          localTyper.atOwner(newOwner).typed(newTree.setSymbol(newSym))
        case _ => x
      }
    }

    def fixOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol, paramSyms: List[Symbol]): Unit = {
      paramSyms match {
        case x :: xs =>
          changeParamSymbols(tree, oldOwner, paramSyms)
        case _ =>
      }
      changeOwner(tree, oldOwner, newOwner)
    }

    // This method is stolen from Mina
    private def findSymbol(name: Name, tpe: Type, paramSyms: List[Symbol]): Option[Symbol] = {
      val r = for (
        // FIXME: 
        // I commented out this statement, because for generics types might change
        // One way to fix this is by passing a List of a pair of old and new symbols.
        // p <- paramSyms if (p.name == name && p.tpe =:= tpe)
        p <- paramSyms if (p.name == name)
      ) yield {
        p
      }
      r match {
        case head :: Nil => Some(head)
        case _ => None
      }
    }

    // This method is stolen from Mina
    private def changeOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): Unit = {
      var list: Map[Symbol, Symbol] = Map.empty
      tree.foreach {
        (x: Tree) =>
          {
            if (goodSymbol(x.symbol)
              && x.symbol.owner == oldOwner) {
              x match {
                case ident: Ident => 
                  ident.symbol = list(ident.symbol)
                case _ =>
                  val ts = x.symbol
                  val ns = x.symbol.cloneSymbol(newOwner)
                  x.symbol = ns
                  list = list + (ts -> ns)
                  changeOwner(x, ts, ns)
              }
            }
          }
      }
    }

    // This method is stolen from Mina
    private def changeParamSymbols(tree: Tree, oldOwner: Symbol, paramSyms: List[Symbol]): Unit = {
      tree.foreach {
        (x: Tree) =>
          {
            if (goodSymbol(x.symbol)
              && x.symbol.owner == oldOwner) {
              val ns = findSymbol(x.symbol.name, x.symbol.tpe, paramSyms)
              x.symbol = ns match {
                case Some(s) => s
                case None => x.symbol
              }
            }
          }
      }
    }
  }
}
