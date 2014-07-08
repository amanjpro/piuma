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

    import renamer.plgn.global._
    

    /**
      * Creates a literal contant.
      *
      * @param const contant value of the literal
      * 
      * @return a Literal contant of the given "cosnt"
      */
    def mkLiteral[T <: AnyVal](const: AnyVal): Literal = {
      Literal(Constant(const))
    }

    /**
      * Creates a literal contant.
      *
      * @param const contant value of the literal
      * 
      * @return a Literal contant of the given "cosnt"
      */
    def mkLiteral(const: String): Literal = {
      Literal(Constant(const))
    }

    /**
      * Creates a literal unit constant.
      *
      * @return a Literal contant of the "Unit"
      */
    def mkUnitLiteral: Literal = {
      Literal(Constant(()))
    }

    /**
      * Creates a tree for method/function appliation (aka method call)
      *
      * @param fun the function/method to be called
      * @param args a list of arguments for the function/method
      *
      * @return a tree of the function/method appliation
      */
    def mkApply(fun: Tree, args: List[Tree]): Apply = {
      localTyper.typed { Apply(fun, args) }.asInstanceOf[Apply]
    }

    /**
      * Creates a tree for a generic method/function appliation 
      * (aka method call)
      *
      * @param fun the function/method to be called
      * @param targs a list of type arguments
      * @param args a list of arguments for the function/method
      *
      * @return a tree of the function/method appliation
      */
    def mkApply(fun: Tree, targs: List[Tree], args: List[Tree]): Apply = {
      targs match {
        case Nil => mkApply(fun, args)
        case _ =>
          val tpapply = TypeApply(fun, targs)
          mkApply(tpapply, args)

      }
    }
    
// ------ Generating Variables -----------------------------------------------------
    // Issue#4 is an open bug about default params, make sure to fix it
    private def mkValOrVar(isVal: Boolean, owner: Symbol, name: TermName, 
            tpe: Type, rhs: Tree, pos: Position, newFlags: Long): ValDef = {
      val newName = if(name.endsWith(' ')) name else name.localName
      val sym = if(isVal) {
        owner.newValue(newName, pos, newFlags)
      } else {
        owner.newVariable(newName, pos, newFlags)
      }
      sym.setInfoAndEnter(tpe)
      val vtree = typer.atOwner(owner) typed {ValDef(sym, rhs)}
      vtree.asInstanceOf[ValDef]
    }
    
    def mkVar(owner: Symbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree, 
            pos: Position = NoPosition, newFlags: Long = 0L): ValDef = {
      mkValOrVar(false, owner, name, tpe, rhs, pos, newFlags)
    }
    
    def mkVal(owner: Symbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree, 
            pos: Position = NoPosition, newFlags: Long = 0L): ValDef = {
      mkValOrVar(true, owner, name, tpe, rhs, pos, newFlags)
    }
    
//    def mkMethodParam(method: MethodSymbol, name: TermName, tpe: Type, rhs: Tree = EmptyTree): ValDef = {
//      val param = mkVal(method, name, tpe, rhs, ) 
//      
//      null
//    }
// ------ Generating Setters and Getters ------------------------------------------
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
        val strSym = owner.newMethodSymbol(gtr.name.setterName, owner.pos.focus, 
                flags)
        val param = strSym.newSyntheticValueParam(vsym.info, TermName("x$1"))
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





    /**
      * Returns a well-typed DefDef (method).
      *
      * @param name the name of the method
      * @param tparams a list of the type parameters of the method
      * @param vaparamss a list of list of parameters of the method
      * @param tpt the tpt (type tree) of the method
      * @param rhs the right-hand-side of the method
      * @param symbol the symbol of the method
      *
      * @return a well typed method
      */
    def mkDefDef(name: TermName, tparams: List[TypeDef],
                  vparamss: List[List[ValDef]], tpt: Tree,
                  rhs: Tree, symbol: Symbol): DefDef = {
      localTyper.typed { 
        DefDef(Modifiers(symbol.flags),
            name, 
            tparams, vparamss, tpt,
            rhs).setSymbol(symbol) 
      }.asInstanceOf[DefDef]
    }
     
    /**
      * Returns a well-typed DefDef (method).
      *
      * @param rhs the right-hand-side of the method
      * @param symbol the symbol of the method
      *
      * @return a well typed method
      */

    def mkDefDef(rhs: Tree, symbol: Symbol): DefDef = {
      localTyper.typed {
        DefDef(symbol, rhs)
      }.asInstanceOf[DefDef]
    }

    /**
      * Returns a well-typed DefDef (method).
      *
      * @param flags the flags of the method
      * @param name the name of the method
      * @param tparams a list of the type parameters of the method
      * @param vaparamss a list of list of parameters of the method
      * @param ret the return type of the method
      * @param rhs the right-hand-side of the method
      * @param owner the owner of the method
      *
      * @return a well typed method
      */
    def mkDefDef(flags: Long, name: TermName, tparams: List[TypeDef],
                  vparamss: List[List[ValDef]], ret: Type,
                  rhs: Tree, owner: Symbol): DefDef = {
      val symbol = owner.newMethodSymbol(
            name, owner.pos.focus, flags)

      val paramSyms = vparamss.flatten.map(_.symbol)
      val tparamSyms = tparams.map(_.symbol)

      val tpe = tparams match {
        case Nil => MethodType(paramSyms, ret)
        case _ => PolyType(tparamSyms, MethodType(paramSyms, ret))
      }

      symbol.setInfoAndEnter(tpe)

      mkDefDef(rhs, symbol)
    }


    
    /**
      * Converts a method to a function.
      *
      * @param defdef the method to be converted to function
      * @owner the owner of the resulted function
      * @apply a call to the method that needs to be converted
      * 
      * @return returns a function that represented the given method
      */
    def def2Function(defdef: DefDef, owner: Symbol, apply: Apply): Function = {

      val outerClass = defdef.symbol.enclClass
      val closureSelect = owner.isClass match {
        case true =>
          Select(This(outerClass), defdef.symbol)
        case false =>
          Ident(defdef.symbol)
      }

      val closureSym = owner.newTermSymbol(
          TermName(tpnme.ANON_FUN_NAME.toString), 
              owner.pos.focus, Flags.SYNTHETIC)



      val closureParamSyms = defdef.vparamss.flatten.map((p) => {
        val temp = closureSym.newValueParameter(
                    TermName(p.symbol.name.toString), 
                    closureSym.pos.focus, p.symbol.flags)
        temp.info = p.symbol.info
        temp
      })


      val closureParams = closureParamSyms.map((x) => {
        ValDef(x, EmptyTree)
      })

      val closureArgs = closureParamSyms.map((x) => Ident(x))

      val closureTpe = TypeRef(outerClass.info, closureSym, 
              closureParamSyms.map(_.info) ++ List(defdef.tpt.tpe))

      closureSym.setInfo(closureTpe)

      val closureRhs = apply.fun match {
        case TypeApply(_, targs) => 
          Apply(TypeApply(closureSelect, targs), closureArgs)
        case _ => Apply(closureSelect, closureArgs)
      }

      Function(closureParams, closureRhs).setSymbol(closureSym)
    }
  }
}
