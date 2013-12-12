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

    // This method is stolen from Mina
    def changeOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): Unit = {
      var list: Map[Symbol, Symbol] = Map.empty
      tree.foreach {
        (x: Tree) =>
          {
            if (hasSymbol(tree)
              && x.symbol.owner == oldOwner) {
              x match {
                case ident: Ident => ident.symbol = list(ident.symbol)
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
  }
}
