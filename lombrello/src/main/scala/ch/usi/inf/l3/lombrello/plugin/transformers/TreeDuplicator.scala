/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

trait TreeDuplicatorCake {

  duplicator: TransformerPluginComponent =>

  import duplicator.plgn._
  trait TreeDuplicator {
    self: duplicator.TransformerComponent =>

    import duplicator.plgn.global._

    def duplicate(x: Tree): Tree = {
      if(goodSymbol(x.symbol) && goodSymbol(x.symbol.owner))
        duplicate(x, x.symbol.owner)
      else
        // TODO: Fix this error reporting
        throw new Exception("Report an error")
    }

    def duplicate(x: Tree, newOwner: Symbol): Tree = {
      // TODO: Implement this
      duplicate(x, x.symbol.owner)
    }

    def duplicate(x: Tree, newName: Name): Tree = {
      if(goodSymbol(x.symbol) && goodSymbol(x.symbol.owner))
        duplicate(x, newName, x.symbol.owner)
      else
        // TODO: Fix this error reporting
        throw new Exception("Report an error")
    }
    def duplicate(x: Tree, newName: Name, newOwner: Symbol): Tree = {
      val newSym = x.symbol.cloneSymbol(newOwner).setName(newName)
      x match {
        case ValDef(mods, name, tpt, rhs) =>
          newSym.setInfoAndEnter(x.symbol.tpe)
          val newTpt = duplicate(tpt, newSym) 
          val newRhs = duplicate(rhs, newSym) 
          localTyper.typed {
            ValDef(mods, newName.toTermName, newTpt, newRhs).setSymbol(newSym)
          }
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val newTparams = tparams.foldLeft(Nil: List[TypeDef])((z, y) => {
            (duplicate(y, y.name, newSym).asInstanceOf[TypeDef]) :: z
          }).reverse
          val newVparamss = vparamss.foldLeft(List[List[ValDef]]())((z1, y1) =>
          {
            (y1.foldLeft(List[ValDef]())((z2, y2) => {
              (duplicate(y2, y2.symbol.name, newSym).asInstanceOf[ValDef]) :: z2
            }).reverse) :: z1
          }).reverse
          val newTpt = duplicate(tpt, newSym)
          val newRhs = duplicate(rhs, newSym)
          val mtpe = MethodType(newVparamss.flatten.map(_.symbol), newTpt.tpe)
          val newTpe = newTparams match {
            case Nil => 
              mtpe
            case _ =>
              PolyType(newTparams.map(_.symbol), mtpe)
          }
          newSym.setInfoAndEnter(x.symbol.tpe)
          localTyper.typed {
            DefDef(mods, newName.toTermName, newTparams, newVparamss, newTpt,
                      newRhs).setSymbol(newSym)
          }
        case ModuleDef(mods, name, impl) =>
          val newImpl = duplicate(impl, newSym).asInstanceOf[Template]
          // TODO: Set info for newSym
          localTyper.typed {
            ModuleDef(mods, newName.toTermName, newImpl).setSymbol(newSym)
          }
        case ClassDef(mods, name, tparams, impl) =>
          val newTparams = tparams.foldLeft(Nil: List[TypeDef])((z, y) => {
            (duplicate(y, y.name, newSym).asInstanceOf[TypeDef]) :: z
          }).reverse
          val newImpl = duplicate(impl, newSym).asInstanceOf[Template]
          // TODO: Set info for newSym
          localTyper.typed {
            ClassDef(mods, newName.toTypeName, newTparams, newImpl).setSymbol(newSym)
          }
        case _ => x
      }
    }

    def fixOwner(tree: Tree, oldOwner: Symbol, 
              newOwner: Symbol, paramSyms: List[Symbol]): Unit = {
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
        // What about knowing if they have the same owner or not? -- Amanj?
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
                  assert(list.contains(ident.symbol), s"${ident.symbol} could not be found. ${ident.pos}")
                  ident.symbol = list(ident.symbol)
                case _ =>
                  val ts = x.symbol
                  val ns = x.symbol.cloneSymbol(newOwner)
                  try {
                    x.symbol = ns
                    list = list + (ts -> ns)
                    changeOwner(x, ts, ns)
                  } catch {
                    case x: UnsupportedOperationException =>
                      x.toString
                  }

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
              ns match {
                case Some(s) =>
                  try {
                    x.symbol = s
                  } catch {
                    case x: UnsupportedOperationException =>
                      x.toString
                  }
                case None => ()
              }
            }
          }
      }
    }
  }
}
