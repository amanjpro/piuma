/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

trait TreeModifiersCake {

  editor: TransformerPluginComponent =>

  import editor.plgn._
  trait TreeModifiers {
    self: editor.TransformerComponent =>

    import editor.plgn.global._


    /**
      * Adds a parameter as the first parameter of the list of 
      * parameters of a method.
      *
      * @param param the parameter to be added
      * @param the method to add the parameter to
      *
      * @return a new method with param added
      */
    def addParam(param: ValDef, method: DefDef): DefDef = {
      val paramSym = param.symbol

      val vparamss = method.vparamss match {
        case Nil => List(List(param))
        case x :: xs => (param :: x) :: xs
      }
      
      val updatedMethod = DefDef(method.mods, method.name, 
                        method.tparams, vparamss, 
                        method.tpt, method.rhs
                      ).setSymbol(method.symbol)

      val mthdInfo = method.symbol.info.asInstanceOf[MethodType]
  
      updatedMethod.symbol.updateInfo(MethodType(
        paramSym :: mthdInfo.params, mthdInfo.resultType))

      localTyper.typed {
        updatedMethod
      }.asInstanceOf[DefDef]

    }

    /**
      * Updates the RHS of a ValDef to a rhs.
      *
      * @param tree the ValDef to be udpated
      * @param rhs the new right-hand-side (or value) of 
      *        the ValDef
      *
      * @return a well-typed ValDef that is identical to tree,
      *         except that it has rhs as its value.
      */
    def updateRHS(tree: ValDef, rhs: Tree): ValDef = {
      val nval = ValDef(tree.mods, tree.symbol.name.toTermName, tree.tpt, 
                  rhs).setSymbol(tree.symbol)
      localTyper.typed {
        nval 
      }.asInstanceOf[ValDef]
    }

    /**
      * Updates the RHS of a method to a rhs.
      *
      * @param tree the method to be udpated
      * @param rhs the new right-hand-side (or value) of 
      *        the method
      *
      * @return a well-typed DefDef that is identical to tree,
      *         except that it has rhs as its value.
      */
    def updateRHS(tree: DefDef, rhs: Tree): DefDef = {
      val nval = DefDef(tree.mods, tree.symbol.name.toTermName, tree.tparams,
                  tree.vparamss, tree.tpt, 
                  rhs).setSymbol(tree.symbol)
      localTyper.typed {
        nval 
      }.asInstanceOf[DefDef]
    }
  }
}


