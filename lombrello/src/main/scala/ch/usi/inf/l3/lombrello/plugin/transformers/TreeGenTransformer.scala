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

    import renamer.plgn._
    import renamer.plgn.global._

    /**
      * Creates an AST node for Return
      *
      * @param expr the expression to be returned
      * 
      * @return a node that represents returning of expr
      */
    def mkReturn(expr: Tree): Return = {
      localTyper.typed {Return(expr)}.asInstanceOf[Return]
    }
    /**
      * Creates an AST node for Assignment
      *
      * @param lhs the tree to be assigned to
      * @param rhs the tree taht represents the value
      * 
      * @return a node that represents assigning rhs to lhs
      */
    def mkAssign(lhs: Tree, rhs: Tree): Assign = {
      localTyper.typed {Assign(lhs, rhs)}.asInstanceOf[Assign]
    }

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
      * @return a Literal constant of the "Unit"
      */
    def mkUnitLiteral: Literal = {
      Literal(Constant(()))
    }

    /**
      * Creates a literal null constant.
      *
      * @return a Literal constant of the "null"
      */
    def mkNullLiteral: Literal = {
      Literal(Constant(null))
    }

    // TODO: What about Selecting types not just terms?
    /**
      * Creates a Select tree.
      *
      * @param qual the symbol of the qualifier of the select.
      * @param name the name of the selected tree
      *
      * @return a well-typed select tree.
      */
    def mkSelect(qual: Symbol, name: String): Select = {
      mkSelect(Ident(qual), TermName(name))
    }
    
    /**
      * Creates a Select tree.
      *
      * @param qual the tree of the qualifier of the select.
      * @param name the name of the selected tree
      *
      * @return a well-typed select tree.
      */
    def mkSelect(qual: Tree, name: String): Select = {
      require(hasSymbol(qual))
      mkSelect(qual, TermName(name))
    }

    /**
      * Creates a Select tree.
      *
      * @param qual the tree of the qualifier of the select.
      * @param name the name of the selected tree
      *
      * @return a well-typed select tree.
      */
    def mkSelect(qual: Tree, name: Name): Select = {
      // FIXME:
      /*
        If the tree that is selected is a generic tree like for example
        List class (or module), then for some reason the typed method 
        returns a TypeApply, that is why I only return the select part
        of it!
      */

      require(hasSymbol(qual)) 
      val s = Select(qual, name).setSymbol(qual.symbol) 
      localTyper.typed { s } match {
        case x: Select => x
        case _ => Select(qual, name)
      }
    }

    /**
      * Creates a Select tree.
      *
      * @param qual the symbol of the qualifier of the select.
      * @param name the name of the selected tree
      *
      * @return a well-typed select tree.
      */
    def mkSelect(qual: Symbol, name: Name): Select = {
      mkSelect(Ident(qual), name)
    }


    /**
      * Creates a block of statements
      *
      * @param stmts the statements of a block
      *
      * @return a well-typed Block instance.
      */
    def mkBlock(stmts: List[Tree]): Tree = {
      stmts match {
        case Nil => mkBlock(Nil, mkUnitLiteral)
        case l => mkBlock(l.take(l.size - 1), l.last)
      }
    }

    /**
      * Creates a block of statements
      *
      * @param stats all but last statements of a block
      * @param ret the last statement of a block
      *
      * @return a well-typed Block instance.
      */
    def mkBlock(stats: List[Tree], ret: Tree): Tree = {
      // FIXME: it is not safe to type-check block then
      // cast it back to block, because of situations as 
      // follows:
      // typed {mkBlock(Nil, Literal(Constant(1))}
      // returns Literal(Constant(1)) not a Block
      typed { Block(stats, ret)}
    }

    /**
      * Creates a Function expression
      *
      * @param params the list of parameters of the function value
      * @param body the body of the function value
      *
      * @return a well-typed Function instance.
      */
    def mkFunction(params: List[ValDef], body: Tree): Function = {
      localTyper.typed(Function(params, body)).asInstanceOf[Function]
    }


    /**
      * Creates an if-else expression
      *
      * @param cond the condition of the if-else
      * @param thenp then clause of the if-else
      * @param elsep else clasue of the if-else
      *
      * @return a well-typed If instance.
      */
    def mkIf(cond: Tree, thenp: Tree, elsep: Tree): If = {
      localTyper.typed(If(cond, thenp, elsep)).asInstanceOf[If]
    }
    /**
      * Creates a CaseDef
      *
      * @param pat the pattern tree of the case
      * @param guard the guard tree of the case
      * @param body the body of the case
      *
      * @return a well-typed CaseDef instance.
      */
    def mkCaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = {
      localTyper.typed(CaseDef(pat, guard, body)).asInstanceOf[CaseDef]
    }

    /**
      * Creates a Match
      *
      * @param selector the selector of the match
      * @param cases the cases of the match
      *
      * @return a well-typed CaseDef instance.
      */
    def mkMatch(selector: Tree, cases: List[CaseDef]): Match = {
      localTyper.typed(Match(selector, cases)).asInstanceOf[Match]
    }

    /**
      * Creates a constructor parameter tree, or a constructor field if neccessary
      *
      * @param constructor tree that represents the constructor
      * @param name the name of the constructor parameter.
      * @param info the type tree of the constructor parameter.
      * @param isField a flag whether this parameter is needs a field or not
      *
      * @return a well-typed constructor parameterer owned by the constructor. If
      *         the isField flag is true, then the parameter is owned by the owner
      *         of the constructor.
      */
    def mkConstructorParam(constructor: DefDef, name: String,
                          info: Type, 
                          isField: Boolean): ValDef = {
      mkConstructorParam(constructor, name, info, EmptyTree, isField) 
    }

    /**
      * Creates a constructor tree
      *
      * @param clazz a tree that represents the host class
      * @param tparams the list of the type parameters of the constructor
      *
      * @return a tree that represents a constructor of class ``clazz''. Note
      *         that the constructor does not have a body, and takes not parameter
      *         to add them, use addParam and updateRhs.
      */
    def mkConstructor(clazz: ClassDef, tparams: List[TypeDef]): DefDef = {
      require(hasSymbol(clazz))
      mkConstructor(clazz.symbol.asClass, tparams)
    }

    /**
      * Creates a constructor tree
      *
      * @param clazz a symbol that represents the host class/module
      * @param tparams the list of the type parameters of the constructor
      *
      * @return a tree that represents a constructor of class ``clazz''. Note
      *         that the constructor does not have a body, and takes not parameter
      *         to add them, use addParam and updateRhs.
      */
    def mkConstructor(clazz: ClassSymbol, tparams: List[TypeDef]): DefDef = {
      require(goodSymbol(clazz))

      val constSym = clazz.newClassConstructor(clazz.pos.focus)
      val mtpe = MethodType(Nil, constSym.tpe)

      val tpe = tparams match {
        case Nil => mtpe
        case xs => PolyType(Nil, mtpe)
      }
      constSym.setInfoAndEnter(tpe)

      typed {DefDef(constSym, Nil, mkBlock(List(mkUnitLiteral)))}.asInstanceOf[DefDef]
    }
    /**
      * Creates a constructor tree
      *
      * @param clazz a tree ot represent the host class
      *
      * @return a tree that represents a constructor of class ``clazz''. Note
      *         that the constructor does not have a body, and takes not parameter
      *         to add them, use addParam and updateRhs.
      */
    def mkConstructor(clazz: ClassDef): DefDef = {
      mkConstructor(clazz, Nil)
    }

    /**
      * Creates a constructor parameter tree, or a constructor field if neccessary
      *
      * @param constructor tree that represents the constructor
      * @param name the name of the constructor parameter.
      * @param info the type tree of the constructor parameter.
      * @param rhs the default value of the constructor parameter
      * @param isField a flag whether this parameter needs a field or not
      *
      * @return a well-typed constructor parameterer owned by the constructor. If
      *         the isField flag is true, then the parameter is owned by the owner
      *         of the constructor.
      */
    def mkConstructorParam(constructor: DefDef, name: String,
                          info: Type, rhs: Tree,
                          isField: Boolean): ValDef = {
      val consSym = constructor.symbol
      goodSymbol(consSym) match {
        case true if isField =>
          val psym = consSym.newValueParameter(
            TermName(name), consSym.pos.focus)
          psym setInfo info
          psym.setFlag(Flag.PARAMACCESSOR | Flag.PARAM | Flag.SYNTHETIC)
          localTyper.typed {
            ValDef(psym, rhs)
          }.asInstanceOf[ValDef]
        case true if goodSymbol(consSym.owner) =>
          val owner = consSym.owner
          val psym = owner.newValueParameter(
                      TermName(name), owner.pos.focus)
          psym setInfoAndEnter info
          psym.setFlag(Flag.PARAMACCESSOR | Flag.PARAM | Flag.SYNTHETIC)
          localTyper.typed {
            ValDef(psym, rhs)
          }.asInstanceOf[ValDef]
        case _ =>
          // TODO what to do?
          throw new Exception("")
      }
    }



    /**
      * Creates a parameter tree.
      *
      * @param name the name of the parameter.
      * @param tpe the type tree of the parameter.
      * @param rhs the default value of the parameter
      * @param owner the symbol of the owner method of the parameter
      *
      * @return a well-typed parameter.
      */
    def mkParam(name: String, tpe: TypeTree, 
                  rhs: Tree, owner: Symbol): ValDef = {
      // TODO: Implement this function
      // owner.newSyntheticValueParam()
      // ValDef(Modfiers(PARAM), TermName(name), tpe, rhs)
      // localTyper.typed {Select(Ident(qual), name) }.asInstanceOf[Select]
      ???
    }

    /**
      * Creates a tree for identifiers
      *
      * @param sym the symbol of the ident
      *
      * @return a tree for the identifier that points to sym.
      */
    def mkIdent(sym: Symbol): Ident = {
      localTyper.typed { Ident(sym).setSymbol(sym) }.asInstanceOf[Ident]
    }



    /**
      * Creates a tree for LabelDef.
      *
      * Labels are usually used for representing {{{while}}} loops.
      *
      * @param name the name of the label
      * @param params the parameters of the label
      * @param body the body of the label
      *
      * @return a tree of the label
      */
    def mkLabel(name: TermName, params: List[Ident], body: Tree): LabelDef = {
      localTyper.typed { LabelDef(name, params, body) }.asInstanceOf[LabelDef]
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

    /**
      * Surrounds {{{body}}} with synchronized block.
      *
      * @param body the tree that needs to be syncrhonized
      * @param on the tree on which the syncrhonized is called
      * @param tpt the type tree of the body
      *
      * @return returns a tree that represents a call of synchronized on
      *         the body.
      */
    def mkSynchronized(body: Tree, on: Tree, tpt: Tree): Apply = {
      mkApply(Select(on, TermName("synchronized")), List(tpt), List(body))
    }
    
    /**
      * Makes a constructor call for a class, i.e. a ``new'' expression
      *
      * @param tpt the symbol of the class that needs to be constructed
      * @param args the list of arguments to be provided
      *
      * @return returns a tree that represents an expression to constructing
      *         an instance of tpt.
      */
    def mkConstructorCall(tpt: Tree, args: List[Tree]): Apply = {
      mkConstructorCall(tpt, Nil, args)
    }

    /**
      * Makes a constructor call for a class, i.e. a ``new'' expression
      *
      * @param tpt the symbol of the class that needs to be constructed
      * @param targs the list of type arguments to be applied
      * @param args the list of arguments to be provided
      *
      * @return returns a tree that represents an expression to constructing
      *         an instance of tpt.
      */
    def mkConstructorCall(tpt: Tree, targs: List[Tree], 
                                  args: List[Tree]): Apply = {
      mkApply(Select(New(tpt), nme.CONSTRUCTOR), targs, args)
    }
    /**
      * Creates a val or var, depending on isVal flag. And it is owned
      * by the owner.
      *
      * @param isVal a flag, if true then the method returns val, and var
      *        otherwise.
      * @param owner the owner of the val/var
      * @param name the name of the val/var
      * @param tpe the type of the val/var
      * @param rhs the right-hand side of the val/var
      *
      * @return returns a ValDef tree
      */
    private def mkValOrVar(isVal: Boolean, owner: Symbol, name: TermName, 
            tpe: Type, rhs: Tree): ValDef = {
      // TODO: Issue#4 is an open bug about default params, make sure to fix it
      val newName = if(name.endsWith(' ')) name else name.localName

      val sym = if(isVal) {
        owner.newValue(newName, owner.pos.focus)
      } else {
        owner.newVariable(newName, owner.pos.focus)
      }


      owner match {
        case x: ClassSymbol =>
          sym setInfoAndEnter tpe
        case y: ModuleSymbol =>
          sym setInfoAndEnter tpe
        case _ =>
          sym setInfo tpe
      } 
      
      val vtree = localTyper.typed {ValDef(sym, rhs)}
      vtree.asInstanceOf[ValDef]
    }
    

    /**
      * Creates a val or var, depending on isVal flag. And it is owned
      * by the owner.
      *
      * @param isVal a flag, if true then the method returns val, and var
      *        otherwise.
      * @param owner the owner of the val/var
      * @param name the name of the val/var
      * @param rhs the right-hand side of the val/var
      *
      * @return returns a ValDef tree
      */
    private def mkValOrVar(isVal: Boolean, owner: Symbol, name: TermName, 
            rhs: Tree): ValDef = {
      val tpe = localTyper.typed {rhs}.tpe
      mkValOrVar(isVal, owner, name, tpe, rhs)
    }

    /**
      * Creates a var which is is owned by the owner.
      *
      * @param owner the owner of the var
      * @param name the name of the var
      * @param tpe the type of the var
      * @param rhs the right-hand side of the var
      *
      * @return returns a ValDef tree
      */
    def mkVar(owner: Symbol, name: TermName, tpe: Type, 
              rhs: Tree): ValDef = {
      mkValOrVar(false, owner, name, tpe, rhs)
    }

    /**
      * Creates a var which is is owned by the owner.
      *
      * @param owner the owner of the var
      * @param name the name of the var
      * @param rhs the right-hand side of the var
      *
      * @return returns a ValDef tree
      */
    def mkVar(owner: Symbol, name: TermName, rhs: Tree): ValDef = {
      mkValOrVar(false, owner, name, rhs)
    }

    /**
      * Creates a var which is is owned by the owner.
      *
      * @param owner the owner of the var
      * @param name the name of the var
      * @param tpe the type of the var
      * @param rhs the right-hand side of the var
      *
      * @return returns a ValDef tree
      */
    def mkVar(owner: Symbol, name: String, tpe: Type, 
              rhs: Tree): ValDef = {
      mkValOrVar(false, owner, TermName(name), tpe, rhs)
    }

    /**
      * Creates a var which is is owned by the owner.
      *
      * @param owner the owner of the var
      * @param name the name of the var
      * @param rhs the right-hand side of the var
      *
      * @return returns a ValDef tree
      */
    def mkVar(owner: Symbol, name: String, rhs: Tree): ValDef = {
      mkValOrVar(false, owner, TermName(name), rhs)
    }

    /**
      * Creates a val which is is owned by the owner.
      *
      * @param owner the owner of the val
      * @param name the name of the val
      * @param tpe the type of the val
      * @param rhs the right-hand side of the val
      *
      * @return returns a ValDef tree
      */
    def mkVal(owner: Symbol, name: TermName, tpe: Type, 
              rhs: Tree): ValDef = {
      mkValOrVar(true, owner, name, tpe, rhs)
    }

    /**
      * Creates a val which is is owned by the owner.
      *
      * @param owner the owner of the val
      * @param name the name of the val
      * @param rhs the right-hand side of the val
      *
      * @return returns a ValDef tree
      */
    def mkVal(owner: Symbol, name: TermName, rhs: Tree): ValDef = {
      mkValOrVar(true, owner, name, rhs)
    }

    /**
      * Creates a val which is is owned by the owner.
      *
      * @param owner the owner of the val
      * @param name the name of the val
      * @param tpe the type of the val
      * @param rhs the right-hand side of the val
      *
      * @return returns a ValDef tree
      */
    def mkVal(owner: Symbol, name: String, tpe: Type, 
              rhs: Tree): ValDef = {
      mkValOrVar(true, owner, TermName(name), tpe, rhs)
    }

    /**
      * Creates a val which is is owned by the owner.
      *
      * @param owner the owner of the val
      * @param name the name of the val
      * @param rhs the right-hand side of the val
      *
      * @return returns a ValDef tree
      */
    def mkVal(owner: Symbol, name: String, rhs: Tree): ValDef = {
      mkValOrVar(true, owner, TermName(name), rhs)
    }
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
      * Returns a well-typed companion object of a class.
      *
      * @param clazz the symbol of the class that needs a companion object
      *
      * @return a well typed companion object of clazz
      */
    def mkCompanionObject(clazz: ClassSymbol): ModuleDef = {
      val modname = clazz.name
      val owner = clazz.owner
      val symb = clazz.newModule(modname.toTermName,
        clazz.pos.focus, Flags.MODULE)
      val msymb = symb.moduleClass
      symb.owner = owner
      msymb.owner = owner

      val parents = List(Ident(definitions.AnyRefClass))
      val mtpe = ClassInfoType(parents.map(_.symbol.tpe), newScope, msymb)

      msymb setInfo mtpe
      symb setInfoAndEnter msymb.tpe

      val const = msymb.newClassConstructor(symb.pos.focus)
      const setInfoAndEnter (MethodType(Nil, symb.info))
      
      val spr = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
      val nbody = Block(List(Apply(spr, Nil)), mkUnitLiteral)
      val cnstrct = DefDef(const, List(Nil), nbody)


      typed {
            ModuleDef(symb, Template(parents, 
                  noSelfType, List(cnstrct)))}.asInstanceOf[ModuleDef]

     


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
      * Returns a well-typed DefDef (method) using an already exisiting
      * DefDef as template.
      *
      * @param template the template method to be used
      * @param name the name of the newly created method
      * @param rhs the body of the newly created method
      *
      * @return a well typed method using an already existing method as
      *         template (here template parameter)
      */
    def mkDefDef(template: DefDef, name: String, rhs: Tree): DefDef = {
      val owner = template.symbol.owner
      val symbol = owner.newMethodSymbol(
            TermName(name), owner.pos.focus, template.symbol.flags)

      val encClassSym = template.symbol.enclClass

      val newMethodSym = encClassSym.newMethodSymbol(
            TermName(name),
            encClassSym.pos.focus,
            template.symbol.flags)

      val newParams = template.vparamss.map(_.map( x => {
         val pSymbolType = x.symbol.tpe;
         val newParamSym = 
              newMethodSym.newSyntheticValueParam(pSymbolType, x.name)
         localTyper.typed(ValDef(newParamSym, 
              x.tpt.duplicate)).asInstanceOf[ValDef]
        })) 


      fixOwner(rhs, template.symbol, newMethodSym, newParams.flatten.map(_.symbol))

      val m = DefDef(Modifiers(template.symbol.flags), 
                      TermName(name), template.tparams,
                      template.vparamss, template.tpt, 
                      rhs).setSymbol(newMethodSym)
      localTyper.typed(m).asInstanceOf[DefDef]
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
