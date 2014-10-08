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
      require(hasSymbol(x))
      val owner = x.symbol.owner match {
        case s: Symbol if goodSymbol(s) && s.isModule =>
          s.moduleClass
        case s => s
      }
      duplicate(x, owner)
    }

    def duplicate(x: Tree, newOwner: Symbol): Tree = {
      duplicate(x, x.symbol.name, newOwner)
    }

    def duplicate(x: Tree, newName: Name): Tree = {
      require(hasSymbol(x))
      val owner = x.symbol.owner match {
        case s: Symbol if goodSymbol(s) && s.isModule =>
          s.moduleClass
        case s => s
      }
      duplicate(x, newName, owner)
    }
    def duplicate(x: Tree, newName: Name, newOwner: Symbol): Tree = {
      val newSym = x.symbol.cloneSymbol(newOwner).setName(newName)
      x match {
        case ValDef(mods, name, tpt, rhs) =>
          val newTpt = tpt.duplicate
          val newRhs = rhs.duplicate

          fixOwner(newRhs, x.symbol, newSym, Nil)
          fixOwner(newTpt, x.symbol, newSym, Nil)

          // try {
          //   newSym.setInfoAndEnter(.tpe)
          // } catch {
          //   case ex: Exception => newSym.setInfo(x.symbol.tpe)
          // }
          localTyper.typed {
            ValDef(mods, newName.toTermName, newTpt, newRhs).setSymbol(newSym)
          }
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val newVparamss = vparamss.map((vparams) => {
            vparams.map((vparam) => {
              val sym = 
                newSym.newSyntheticValueParam(vparam.tpe, vparam.name.toTermName)
              sym.setInfo(vparam.symbol.tpe)
              ValDef(sym, vparam.tpt)
            })
          })
          val newParamSyms = newVparamss.flatten.map(_.symbol)

          val newTpt = tpt.duplicate
          val newRhs = rhs.duplicate


          val mtpe = MethodType(newParamSyms, newTpt.tpe)
          val newTpe = tparams match {
            case Nil => 
              mtpe
            case _ =>
              PolyType(tparams.map(_.symbol), mtpe)
          }

          if(newOwner.info.decls.lookup(newSym.name) == NoSymbol) {
            newSym.setInfoAndEnter(newTpe)
          } else {
            newSym.setInfo(newTpe)
          }


          fixOwner(newRhs, x.symbol, newSym, newParamSyms)
          
          val typedRhs = typed { newRhs } 
          localTyper.typed {
            val methdef = DefDef(newSym, newVparamss, typedRhs)
            methdef.tpt setType localTyper.packedType(typedRhs, newSym)
            methdef
          }


        case ModuleDef(mods, name, impl) =>
          val newImpl = duplicate(impl, newSym).asInstanceOf[Template]
          // TODO: Set info for newSym
          localTyper.typed {
            ModuleDef(mods, newName.toTermName, newImpl).setSymbol(newSym)
          }
        case clazz @ ClassDef(mods, name, tparams, impl) =>
          // val newTparams = tparams.foldLeft(Nil: List[TypeDef])((z, y) => {
          //   (duplicate(y, y.name, newSym).asInstanceOf[TypeDef]) :: z
          // }).reverse

          val dbody = impl.duplicate.body
          val nparents = impl.parents.map(_.symbol.tpe)
          val ntpe = ClassInfoType(nparents, newScope, newSym)

          if(newOwner.info.decls.lookup(newSym.name) == NoSymbol) {
            newSym.setInfoAndEnter(ntpe)
          } else {
            newSym.setInfo(ntpe)
          }


          val ths = This(newSym)
          ths.setType(ntpe)

          val newBody = dbody.map(_.substituteThis(clazz.symbol, ths))


          newBody.foreach(fixOwner(_, clazz.symbol, newSym))
          localTyper.typed {
            ClassDef(newSym, Template(impl.parents, impl.self, newBody))
          }
        case _ => x
      }
    }

    def fixOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): Unit = {
      fixOwner(tree, oldOwner, newOwner, Nil)
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
    private def findSymbol(name: Name, tpe: Type, 
      paramSyms: List[Symbol]): Option[Symbol] = {
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
                  assert(list.contains(ident.symbol), 
                        s"${ident.symbol} could not be found. ${ident.pos}")
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
    private def changeParamSymbols(tree: Tree, oldOwner: Symbol, 
            paramSyms: List[Symbol]): Unit = {
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
