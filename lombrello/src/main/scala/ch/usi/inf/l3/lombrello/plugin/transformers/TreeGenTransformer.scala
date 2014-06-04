/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags


trait TreeGenTransformerCake {
  renamer: TransformerPluginComponent =>
  trait TreeGenTransformer {
    self: renamer.TransformerComponent =>

    import renamer.global._
    
    
// ------ Generating Variables -------------------------------------------------------------------
    // Issue#4 is an open bug about default params, make sure to fix it
    private def mkValOrVar(isVal: Boolean, owner: Symbol, name: TermName, tpe: Type, rhs: Tree, pos: Position, newFlags: Long): ValDef = {
      val newName = if(name.endsWith(' ')) name else nme.getterToLocal(name)
      val sym = if(isVal) {
        owner.newValue(newName, pos, newFlags)
      } else {
        owner.newVariable(newName, pos, newFlags)
      }
      sym.setInfoAndEnter(tpe)
      val vtree = typer.atOwner(owner) typed {ValDef(sym, rhs)}
      vtree.asInstanceOf[ValDef]
    }
    
    def mkVar(owner: Symbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree, pos: Position = NoPosition, newFlags: Long = 0L): ValDef = {
      mkValOrVar(false, owner, name, tpe, rhs, pos, newFlags)
    }
    
    def mkVal(owner: Symbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree, pos: Position = NoPosition, newFlags: Long = 0L): ValDef = {
      mkValOrVar(true, owner, name, tpe, rhs, pos, newFlags)
    }
    
//    def mkMethodParam(method: MethodSymbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree): ValDef = {
//      val param = mkVal(method, name, tpe, rhs, ) 
//      
//      null
//    }
// ------ Generating Setters and Getters -------------------------------------------------------------------
    private def accessorFlags(vsym: Symbol): Long = {
      if(vsym.isParamAccessor) {
        Flags.STABLE | Flags.ACCESSOR | Flags.PARAMACCESSOR
      } else {
        Flags.STABLE | Flags.ACCESSOR
      }
    }
    
    def mkSetterAndGetter(tree: ValDef): Option[(DefDef, DefDef)] = {
      val vsym = tree.symbol
      val gtr = mkGetter(tree)
      val owner = vsym.owner
      if(vsym.isVar) {
        val flags = accessorFlags(vsym)
        val strSym = owner.newMethodSymbol(nme.getterToSetter(gtr.name), owner.pos.focus, flags)
        val param = strSym.newSyntheticValueParam(vsym.info, "x$1")
        val tpe = MethodType(List(param), definitions.UnitTpe)
        strSym.setInfoAndEnter(tpe)
        val rhs = Assign(Select(This(owner), vsym.name), Ident(param))
        val str = typer.atOwner(owner) typed {DefDef(strSym, rhs)}
        Some((gtr, str.asInstanceOf[DefDef]))
      } else
        None
    }
    
    
    def mkGetter(tree: ValDef): DefDef = {
      val vsym = tree.symbol
      val owner = vsym.owner
      val flags = accessorFlags(vsym)
      
      val gtrSym = owner.newMethodSymbol(vsym.getterName, owner.pos.focus, flags)
      val tpe = NullaryMethodType(vsym.info)
      gtrSym.setInfoAndEnter(tpe)
      val rhs = Select(This(owner), vsym.name) 
      val gtr = typer.atOwner(owner) typed {DefDef(gtrSym, rhs)}
      gtr.asInstanceOf[DefDef]
    }
  }
}
