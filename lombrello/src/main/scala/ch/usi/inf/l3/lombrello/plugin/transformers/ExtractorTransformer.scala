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


    /**
      * Returns the free variables in a statement.
      *
      * @param stmt the statement that might contain
      *        free variables
      * @param captured captured variables used in the stmt
      * @param acc free variables in stmt found so far
      *
      * @return a list of the symbols of the free variables in stmt
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def findFreeVars(stmt: Tree,
                              captured: List[Symbol],
                              acc: List[Symbol]): List[Symbol] = {
      // TODO: Implement this
      stmt match {
        case Alternative(trees) =>
          findFreeVars(trees, captured, acc)
        case Annotated(annot, arg) =>
          val temp = findFreeVars(annot, captured, acc)
          findFreeVars(arg, captured, temp)
        case AppliedTypeTree(tpt, args) =>
          val temp = findFreeVars(tpt, captured, acc)
          findFreeVars(args, captured, temp)
        case Apply(fun, args) =>
          val temp = findFreeVars(fun, captured, acc)
          findFreeVars(args, captured, temp)
        case Assign(lhs, rhs) =>
          val temp = findFreeVars(lhs, captured, acc)
          findFreeVars(rhs, captured, temp)
        case Bind(name, body) =>
          findFreeVars(body, captured, acc)
        case Block(stats, expr) =>
          val temp = findFreeVars(stats, captured, acc)
          findFreeVars(expr, captured, temp)
        case CaseDef(pat, guard, body) =>
          // TODO: What about binding new names?
          val temp1 = findFreeVars(pat, captured, acc)
          val temp2 = findFreeVars(guard, captured, temp1)
          findFreeVars(body, captured, temp2)
        case ClassDef(mods, name, tparams, impl) =>
          findFreeVars(impl, captured, acc)
        case CompoundTypeTree(templ) =>
          findFreeVars(templ, captured, acc)
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          findFreeVars(rhs, captured, acc)
        case ExistentialTypeTree(tpt, whereClause) =>
          val temp = findFreeVars(tpt, captured, acc)
          findFreeVars(whereClause, captured, temp)
        case Function(vparams, body) =>
          findFreeVars(body, captured, acc)
        case If(cond, thenp, elsep) =>
          val temp1 = findFreeVars(cond, captured, acc)
          val temp2 = findFreeVars(thenp, captured, temp1)
          findFreeVars(elsep, captured, temp2)
        case Import(expr, selectors) =>
          findFreeVars(expr, captured, acc)
        case LabelDef(name, params, rhs) =>
          val temp = findFreeVars(params, captured, acc)
          findFreeVars(rhs, captured, temp)
        case Literal(value) =>
          acc
        case Match(selector, cases) =>
          val temp = findFreeVars(selector, captured, acc)
          findFreeVars(cases, captured, temp)
        case ModuleDef(mods, name, impl) =>
          findFreeVars(impl, captured, acc)
        case New(tpt) =>
          acc
        case PackageDef(pid, stats) =>
          findFreeVars(stats, captured, acc)
        case RefTree(qual, name) =>
          findFreeVars(qual, captured, acc)
        case ReferenceToBoxed(ident) =>
          findFreeVars(ident, captured, acc)
        case Return(expr) =>
          findFreeVars(expr, captured, acc)
        case Select(qual, name) =>
          findFreeVars(qual, captured, acc)
        case SelectFromTypeTree(qualifier, name) =>
          findFreeVars(qualifier, captured, acc)
        case SingletonTypeTree(ref) =>
          findFreeVars(ref, captured, acc)
        case Star(elem) =>
          findFreeVars(elem, captured, acc)
        case Super(qual, mix) =>
          findFreeVars(qual, captured, acc)
        case Template(parents, self, body) =>
          val temp = findFreeVars(self, captured, acc)
          findFreeVars(body, captured, temp)
        case This(qual) =>
          acc
        case Throw(expr) =>
          findFreeVars(expr, captured, acc)
        case Try(block, catches, finalizer) =>
          val temp1 = findFreeVars(block, captured, acc)
          val temp2 = findFreeVars(catches, captured, temp1)
          findFreeVars(finalizer, captured, temp2)
        case TypeApply(fun, args) =>
          val temp = findFreeVars(fun, captured, acc)
          findFreeVars(args, captured, temp)
        case TypeBoundsTree(lo, hi) =>
          val temp = findFreeVars(lo, captured, acc)
          findFreeVars(hi, captured, temp)
        case TypeDef(mods, name, tparams, rhs) =>
          val temp = findFreeVars(tparams, captured, acc)
          findFreeVars(rhs, captured, temp)
        case x: TypeTree =>
          acc
        case Typed(expr, tpt) =>
          val temp = findFreeVars(expr, captured, acc)
          findFreeVars(tpt, captured, temp)
        case UnApply(fun, args) =>
          val temp = findFreeVars(fun, captured, acc)
          findFreeVars(args, captured, temp)
        case v @ ValDef(mods, name, tpt, rhs) =>
          // v cannot have Symbol because of 
          // {{{ findFreeVars(List[Tree], List[Symbol], 
          //       List[Symbol]): List[Symbol] }}}
          findFreeVars(rhs, captured, acc) 
        case x: Ident 
           if ! acc.contains(x.symbol) && 
              ! captured.contains(x.symbol) =>
          (x.symbol :: acc)
        case x: Ident =>
          acc
      }
    }

    /**
      * Returns the free variables in a list of statements.
      *
      * @param stmts a list of statements that might contain
      *        free variables
      * @param captured captured variables used in the stmts
      * @param acc free variables in stmts found so far
      *
      * @return a list of the symbols of the free variables in stmts
      */
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
      * Returns a list of parameter trees
      *
      * @param paramSyms a list of the symbols of the parameters
      *
      * @return a list of ValDef which represent the parameters of the extracted
      *         method.
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def generateParams(paramSyms: List[Symbol]): List[ValDef] = {
      paramSyms.map((x) => ValDef(x, EmptyTree))
    }


    /**
      * Returns the symbols of the extracted method parameters, and
      * the arguments to call the method.
      *
      * @param extractedMethod the symbol of the extracted method
      * @param freeVars the list of the found free variable symbols
      *
      * @return A tuple of a list of Symbols of the parameters and
      *         a list of the argument trees to call the extracted
      *         method.
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def generateParamSymsAndArgs(extractedMethod: Symbol, 
          freeVars: List[Symbol]): (List[Symbol], List[Tree]) = {
      val paramSyms = freeVars.map(
        (x) => {
          val temp = 
            extractedMethod.newSyntheticValueParam(x.info, 
                        TermName(x.name.toString))
          temp.setAnnotations(x.annotations)
          temp
        })

      val args = paramSyms.foldLeft(List.empty[Tree])((z, y) => {
        val id = Ident(freeVars.find((x) => y.name == x.name).get)
        localTyper.typed(id) :: z
      }).reverse
      (paramSyms, args)
    }



    /**
      * Returns a list of type parameter trees
      *
      * @param tparamSyms a list of the symbols of the type parameters
      *
      * @return a list of TypeDef which represent the type parameters of 
      *         the extracted method.
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def generateTParams(tparamSyms: List[Symbol]): List[TypeDef] = {
      tparamSyms.map((x) => TypeDef(x))
    }

    /**
      * Returns the symbols of the extracted method type parameters, and
      * the type arguments to call the method.
      *
      * @param extractedMethod the symbol of the extracted method
      * @param freeTParams the list of the found free type parameter symbols
      *
      * @return A tuple of a list of Symbols of the type parameters and
      *         a list of the type argument trees to call the extracted
      *         method.
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def generateTParamSymsAndTArgs(freeTParams: List[Symbol], 
        extractedMethod: Symbol, paramSyms: List[Symbol]): 
              (List[Symbol], List[Tree]) = {
      val commonTparams = 
          freeTParams.filter((s) => paramSyms.exists(s.tpe =:= _.info))
      val newTargs = commonTparams.map((x) => TypeTree(x.tpe))
      val tparamSyms = commonTparams.map(
        (x) => {
          val nsym = extractedMethod.newTypeParameter(
              unit.freshTypeName("K"), 
              extractedMethod.pos.focus, x.flags)

          nsym.info = x.info
          paramSyms.foreach((y) => {
            y.substInfo(List(x), List(nsym))
          })
          nsym
        })
      (tparamSyms, newTargs)
    }

    /**
      * Returns the tpe of the extracted method
      *
      * @param newTparamSyms the symbols of the type parameters 
      *        of the extracted method
      * @param paramSyms the symbols of the parameters 
      *        of the extracted method
      * @param ret the return type of the extracted method
      *
      * @return the tpe of the extracted method
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def generateTpe(newTparamSyms: List[Symbol], 
            paramSyms: List[Symbol], ret: Type): Type = {
      newTparamSyms match {
        case Nil => MethodType(paramSyms, ret)
        case _ => PolyType(newTparamSyms, MethodType(paramSyms, ret))
      }
    }

    /**
      * Returns a new tree such that it does not mutate parameters
      *
      * @param tree the tree that might mutate parameters
      *
      * @return A tree that does not mutate parameters
      * @see extractMethod(List[Tree], Symbol, TermName): Option[(DefDef, 
      *                       Option[Apply])]
      */
    private def fixMutatingParams(tree: Block): Block = {
      var aliases: Map[Symbol, Symbol] = Map.empty

      def substituteMutation(x: Tree): Tree = {
        x match {
          case t if (aliases.contains(t.symbol)) =>
            t.setSymbol(aliases(t.symbol))
          case Assign(lhs, rhs) if (isParam(lhs.symbol)) =>
            aliases.contains(lhs.symbol) match {
              case true =>
                Assign(Ident(aliases(lhs.symbol)), rhs)
              case false =>
                val newsym = lhs.symbol.owner.newVariable(
                  unit.freshTermName(lhs.symbol.name + ""),
                lhs.symbol.owner.pos.focus, Flags.MUTABLE)
                newsym.info = lhs.symbol.info
                val newValDef = ValDef(newsym, rhs)
                aliases = aliases + (lhs.symbol -> newsym)
                newValDef
            }
          case t =>
            t.substituteSymbols(aliases.keys.toList, aliases.values.toList)
            t
        }
      }
      val newStats = tree.stats.foldLeft(List.empty[Tree])((z, y) => {
        z ++ List(substituteMutation(y))
      })

      val newExpr = substituteMutation(tree.expr)

      Block(newStats, newExpr)
    }

    /**
      * Extracts a method out of a list of statements, and replaces them
      * with a call to the method.
      *
      * @param stmts the list of statements to be extracted to a method,
      *        and substituted with a method call
      * @param currentOwner the owner of the statements that need to be extracted
      *        to a method
      * @param methodName the name of the extracted method
      *
      * @return Some tuple of the extracted method and a call to that method,
      *         or None if the list is empty.
      */
    def extractMethod(stmts: List[Tree],
                      currentOwner: Symbol, 
                      methodName: TermName): Option[(DefDef, Option[Apply])] = {
      stmts match {
        case Nil => None
        case xs if !goodSymbol(currentOwner) => None
        case xs =>
          // TODO: set proper flags
          val flags = 0
          val mowner = currentOwner.owner
          val msymbol = mowner.newMethodSymbol(
            methodName, mowner.pos.focus, flags)
          val freeVars = findFreeVars(stmts, Nil, Nil)
          val (paramSyms, args) = generateParamSymsAndArgs(msymbol, freeVars)
          val rhs = stmts match {
            case x :: Nil =>
              Block(Nil, x)
            case _ =>
              Block(stmts.dropRight(1), stmts.last)
          }
          fixOwner(rhs, currentOwner, msymbol, paramSyms)

          val params: List[ValDef] = generateParams(paramSyms)

          val freeTparams = currentOwner.tpe match {
            case PolyType(tps, _) =>
              // TODO you should also remove the unused type parameters
              tps
            case _ =>
              Nil
          }
          val (newTparamSyms, newTargs) = generateTParamSymsAndTArgs(
              freeTparams, msymbol, paramSyms)
          val tparams = generateTParams(newTparamSyms)

          val mthdTpe = generateTpe(newTparamSyms, paramSyms, rhs.expr.tpe)
          msymbol.setInfoAndEnter(mthdTpe)
          val tpt = TypeTree(rhs.expr.tpe)

          
          val mthd = DefDef(Modifiers(flags), methodName, tparams,
                  List(params), tpt,
                  fixMutatingParams(rhs)).setSymbol(msymbol)


          val mname = Ident(msymbol)
          val tpapply = tparams match {
            case Nil => mname
            case _ => TypeApply(mname, newTargs)
          }

          val apply = Apply(tpapply, args)


          localTyper.typed { mthd }.asInstanceOf[DefDef]
          localTyper.typed { apply }.asInstanceOf[Apply]

          Some(mthd, Some(apply))
      }
    }
  }
}
