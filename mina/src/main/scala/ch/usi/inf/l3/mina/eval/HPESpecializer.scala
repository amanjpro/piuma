/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.mina.eval

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.tools.nsc.symtab.Flags._
import scala.language.implicitConversions
import ch.usi.inf.l3.mina._
import scala.reflect.internal.ModifierFlags



  


@phase("mina-specializer") class HPESpecializer {

  plugin HPE

  rightAfter(finderPhase)
  before(List(finalizer))

  // FIXME
  // Still no support for generic methods, but supporting them will be straight forward?!
  // FIXME Apply, and Select should update the env of the variable
  def transform(tree: Tree): Tree = {
    //Partially evaluate the program!
    val (newTree, _, env2) = peval(tree, env)
    env = env2
    super.transform(newTree)
  }

  

  private val fevalError = "Blocks marked as CT shall be completely " +
    "known and available at compilation time."

  /*
   * In order to know about a tree, write it in Scala and run the scalac
   * using this command:
   * scalac -Yshow-trees -Xprint:parser YOURPROGRAM
   * 
   */
  private def feval(tree: Tree, env: Environment): (CTValue, Environment) = {
    tree match {
      case v: Literal => (CTValue(HPELiteral(v, v.tpe)), env)
      case v: Ident =>
        env.getValue(v.symbol) match {
          case x: CTValue => (x, env)
          case _ => fail(fevalError + " ident " + v)
        }
      case v @ ValDef(mods, name, tpt, rhs) =>
        val (r, env2) = feval(rhs, env)
        (r, env2.addValue(v.symbol, r))
      case Assign(lhs, rhs) =>
        val (rhs1, env1) = feval(rhs, env)
        (rhs1, env.addValue(lhs.symbol, rhs1))
      case Block(stats, expr) =>
        val env2 = stats.foldLeft(env)((z, y) => {
          val (_, envTemp) = feval(y, z)
          envTemp
        })
        feval(expr, env2)
      case If(cond, thenp, elsep) =>
        val (cond1, env1) = feval(cond, env)
        cond1.toTree match {
          case Literal(Constant(true)) => feval(thenp, env1)
          case Literal(Constant(false)) => feval(elsep, env1)
          case _ => fail(fevalError + " if " + cond)
        }
      case m @ Match(selector, cases) =>

        def matched(cse: CaseDef, env: Environment, mat: CTValue): Boolean = {
          cse.pat match {
            case a @ Alternative(alts) =>
              val fevaledAlts = for (alt <- alts) yield {
                val (r, _) = feval(alt, env)
                r
              }
              fevaledAlts.contains(mat)
            case Ident(nme.WILDCARD) => false
            case _ =>
              val (pat, _) = feval(cse.pat, env)
              pat == mat
          }
        }
        val (r1, env2) = feval(selector, env)

        val (vOption, env3) = 
          cases.foldLeft((None: Option[CTValue], env2))((z, y) => {
          if(z._1 == None && matched(y, z._2, r1)) {
            val t = feval(y.body, env2)
            (Some(t._1), t._2)
          }
          else z
        })
        vOption match {
          case None  =>
            val last = cases.last
            last.pat match {
              case Ident(nme.WILDCARD) => feval(last.body, env2)
              case _ => fail(fevalError + " match-wildcard " + m)
            }
          case Some(v) => (v, env3)
        }
      case Typed(exp, t2) => feval(exp, env)
      case Function(vparams, body) =>
        feval(body, env)

      /*
       * An extractor class to create and pattern match with syntax New(tpt).
       * This AST node corresponds to the following Scala code:
       *
       * new T
       *
       * This node always occurs in the following context:
       *
       * {{{(new tpt).<init>[targs](args)}}}
       *
       * For example, an AST representation of:
       *
       * new Example[Int](2)(3)
       *
       * is the following code:
       *
       * Apply( Apply( TypeApply( Select(New(TypeTree(typeOf[Example])),
       * 					nme.CONSTRUCTOR) TypeTree(typeOf[Int])),
       *      			List(Literal(Constant(2)))),
       *         			List(Literal(Constant(3))))
       */


      case cnstrct @ Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
        digraph.getClassRepr(tpt.tpe) match {
          case Some(clazz) =>
            val mtree = clazz.getMemberTree(nme.CONSTRUCTOR,
              cnstrct.symbol.tpe)
            mtree match {
              case methodTree: DefDef =>
                val (fevaledArgs, env1) = fevalArgs(args, env)
                val params = methodTree.vparamss.flatten.map(_.symbol)
                val funStore = Environment((params, fevaledArgs))
                val (v, env2) = feval(methodTree.rhs, funStore)
                val obj = HPEObject(cnstrct, clazz.tpe, env2)
                (CTValue(obj), env1)
              case _ => fail(fevalError + " constructor " + mtree)
            }

          case None => fail(s"${fevalError} constructor ${tpt}")
        }

      case Return(expr) => feval(expr, env)

      /*
       * An extractor class to create and pattern match with syntax
       * LabelDef(name, params, rhs).
       * This AST node does not have direct correspondence to Scala code.
       * It is used for tailcalls and like. For example, while/do are
       * desugared to label defs as follows:
       *
       * {{{while (cond) body ==> LabelDef($L, List(),
       *                              if (cond) { body; L$() } else ())}}}
       *
       * {{{do body while (cond) ==> LabelDef($L, List(),
       * 						body; if (cond) L$() else ())}}}
       */
      case LabelDef(name, params, rhs) =>
        feval(rhs, env)
      case ths @ This(n) =>
        (CTValue(HPETree(ths)), env)
      case select @ Select(ths @ This(n), name) =>
        val tree = getMemberTree(ths.symbol.tpe, name, select.symbol.tpe)
        feval(tree, env)
      // Unary operations
      case select @ Select(qual, name) if (isUnary(select)) =>
        val (r1, env1) = feval(qual, env)
        doUnaryOperation(name, r1, env1)
      case select @ Select(qual, name) =>
        if (!isPackage(qual)) {
          val (r1, env1) = feval(qual, env)
          digraph.getClassRepr(qual.symbol.owner.tpe) match {
            case Some(repr) =>
              val member = repr.getMemberTree(name, select.symbol.tpe)
              r1.v match {
                case tree: HPEObject =>
                  val (r, nenv) = feval(member, tree.store)
                  val nobj = CTValue(tree.copy(store = nenv))
                  (r, env.updateValue(tree.tree.symbol, nobj))
                case _ => fail(fevalError + " select " + r1)
              }
            case None => fail(s"${fevalError} select ${select}")
          }
        } else {
          (CTValue(HPETree(select)), env)
        }
      case Apply(fun, t) if (fun.symbol.name == newTermName("RT")) =>
        fail(fevalError)
      case Apply(fun, t) if (fun.symbol.name == newTermName("CT")) =>
        feval(t.head, env)
      // Binary operations
      case apply @ Apply(fun @ Select(r, l), args) if (isBinary(apply)) =>
        val arg1 = args.head
        val (r1, env1) = feval(r, env)
        val (arg11, env2) = feval(arg1, env1)

        doBinaryOperation(fun.symbol.name, r1, arg11, env2)
      // If {{{super}}} was {{{Object}}} then we just don't bother
      // executing its constructor
      // TODO once we build a framework to read binary classes to an AST tree
      // we can get rid of this
      case apply @ Apply(fun, args) if (isAnyConstructor(apply)) =>
        val tr = typed(q"new Object()")
        (CTValue(HPEObject(tr, tr.tpe, Environment.empty)), env)
      case apply @ Apply(fun @ Select(obj, m), args) =>
        val (obj2, env2) = feval(obj, env)
        val cntxt = obj2 match {
          case CTValue(o: HPEObject) =>
            o.store
          case _ => env2
        }
        val reciever = obj.symbol.tpe
        val method = fun.symbol
        val (r, e, st) = fevalApply(reciever, method, args, env2, cntxt)
        val re = obj2 match {
          case CTValue(o: HPEObject) =>
            e.updateValue(o.tree.symbol, CTValue(o.copy(store = st)))
          case _ => e
        }
        (r, re)
      case apply @ Apply(fun, args) if (apply.symbol.isStatic) =>
        val reciever = fun.symbol.owner.tpe
        digraph.getClassRepr(reciever) match {
          case Some(clazz) =>
            val (_, _, env2) = peval(clazz.tree, Environment.empty)
            val method = fun.symbol
            val (r, e, _) = fevalApply(reciever, method, args, env2, env2)
            (r, e)
          case None =>
            fail(s"${fevalError} apply ${reciever}")
        }
      case apply @ Apply(fun, args) =>
        val reciever = fun.symbol.owner.tpe
        val method = fun.symbol
        val (r, e, _) = fevalApply(reciever, method, args, env, env)
        (r, e)
      case x => fail(s"${fevalError} ${x.tpe} otherwise ${x}")
    }
  }


  private def peval(tree: Tree, env: Environment): (Tree, Value, Environment) = {
    tree match {
      case clazz: ImplDef =>
        var newEnv = Environment.empty
        var (newBody, _) = 
          clazz.impl.body.foldLeft((Nil: List[Tree], env))((z, y) => {
            val (p, _, e) = peval(y, z._2)
            (z._1 ++ List(p), e)
          })
        (clazz.updateBody(newBody), Top, env)
      case method @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (rhs1, _, temp) = peval(rhs, env)
        val mthd = method.updateRHS(rhs1)
        (mthd, Top, temp)
      case v: Literal =>
        (v, AbsValue(HPELiteral(v, v.tpe)), env)
      case v: Ident =>
        val value = env.getValue(v.symbol)
        val t: Tree = value match {
          case CTValue(_) => value.value
          case _ => v
        }
        (t, value, env)
      case v @ ValDef(mods, name, tpt, rhs) if (!rhs.isEmpty) =>
        // We won't partially evaluate method parameters and 
        // uninitialized (val/var)s
        val (rtree, r, env2) = peval(rhs, env)
        r match {
          case CTValue(_) =>
            (EmptyTree, r, env2.addValue(v.symbol, r))
          case _ =>
            (v.updateRHS(rtree), r, env2.addValue(v.symbol, r))
        }
      case a @ Assign(lhs, rhs) =>
        env.getValue(lhs.symbol) match {
          case CTValue(_) =>
            val (r, env1) = feval(rhs, env)
            val env2 = env1.addValue(lhs.symbol, r)
            val assgn = Assign(lhs, r.v.tree)
            (r.v.tree, r, env2)
          case AbsValue(_) =>
            val (rtree, rhs1, env1) = peval(rhs, env)
            val env2 = env1.addValue(lhs.symbol, rhs1)
            val assgn = Assign(lhs, rtree)
            (assgn, rhs1, env2)
          case Top =>
            val (rtree, rhs1, env1) = peval(rhs, env)
            val assgn = Assign(lhs, rtree)
            (assgn, Top, env1)
          case Bottom =>
            val (rtree, rhs1, env1) = peval(rhs, env)
            val env2 = env1.addValue(lhs.symbol, rhs1)
            rhs1 match {
              case CTValue(x) =>
                (x.tree, rhs1, env2)
              case _ =>
                val assgn = Assign(lhs, rtree)
                (assgn, rhs1, env2)
            }
        }
      case m @ Match(selector, cases) =>
        def matched(cse: CaseDef, env: Environment, mat: Value): Boolean = {
          cse.pat match {
            case Alternative(alts) =>
              val pevaledAlts = for (alt <- alts) yield {
                val (_, r, _) = peval(alt, env)
                r
              }
              pevaledAlts.contains(mat)
            case Ident(nme.WILDCARD) => false
            case _ =>
              val (pat, v, env3) = peval(cse.pat, env)
              if (v.value == mat.value) true
              else false
          }
        }
        val (r1, v, env2) = peval(selector, env)
        v match {
          case CTValue(_) | AbsValue(_) =>
            var continue = true
            val rs = for (
              cse <- cases;
              if continue && matched(cse, env2, v)
            ) yield {
              continue = false
              peval(cse.body, env2)
            }
            rs match {
              case Nil =>
                val last = cases.last
                last.pat match {
                  case Ident(nme.WILDCARD) => peval(last.body, env2)
                  case _ => fail("No match found exception " + m)
                }
              case _ => rs.head
            }
            // TODO: Why this doesn't work
            // val rs = cases.foldLeft((EmptyTree: Tree, 
            //         Bottom: Value, env2))((z, y) => {
            //   (matched(y, env2, v), z._1, z._2) match {
            //     case (true, EmptyTree, Bottom) => 
            //       peval(y, env2)
            //     case _ =>
            //       z
            //   }
            // })
            // rs match {
            //   case (EmptyTree, Bottom, _) =>
            //     val last = cases.last
            //     last.pat match {
            //       case Ident(nme.WILDCARD) => peval(last.body, env2)
            //       case _ => fail("No match found exception " + m)
            //     }
            //   case _ => rs
            // }
          case _ =>
            val rs = for (cse <- cases) yield {
              val (ncse, vncse, envt) = peval(cse.body, env2)
              vncse match {
                case CTValue(_) | AbsValue(_) =>
                  val x = vncse.value.get
                  (cse.updateBody(x.tree), envt)
                case Top | Bottom =>
                  (cse.updateBody(ncse), envt)
              }
            }
            val (newCases, newEnvs) = rs.unzip
            val newMatch = r1.asInstanceOf[Match].updateCases(newCases)
            (newMatch, Top, env2.makeConsistent(newEnvs))
        }
        case block @ Block(stats, expr) =>
          val (stats2, env2) = stats.foldLeft((Nil: List[Tree], env))((z, y) => {
            val (t, _, envTemp) = peval(y, z._2)
            (z._1 ++ List(t), envTemp)
          })
          val (expr2, v2, env3) = peval(expr, env2)
          // TODO: Why ths does not work???
          // val block2 = mkBlock(stats2, expr2)
          val block2 = treeCopy.Block(block, stats2, expr2)
          (block2, v2, env3)
      case ifelse @ If(cond, thenp, elsep) =>
        val (r, v, env1) = peval(cond, env)
        v match {
          case CTValue(HPELiteral(Literal(Constant(true)), _))
            | AbsValue(HPELiteral(Literal(Constant(true)), _)) =>
            peval(thenp, env1)
          case CTValue(HPELiteral(Literal(Constant(false)), _))
            | AbsValue(HPELiteral(Literal(Constant(false)), _)) =>
            peval(elsep, env1)
          case _ =>
            val (tr, tv, tenv) = peval(thenp, env1)
            val (fr, fv, fenv) = peval(elsep, env1)
            val env2 = env1.makeConsistent(List(tenv, fenv))
            val newIf = mkIf(r, tr, fr)
            (newIf, Top, env2)
        }
      case fnctn @ Function(vparams, body) =>
        val (r, _, env2) = peval(body, env)
        val fnctn2 = mkFunction(vparams, r)
        (fnctn2, Top, env2)
      case cnstrct @ Apply(n @ Select(nw @ New(tpt), nme.CONSTRUCTOR), args) =>
        val (trees, vs, env2) = pevalArgs(args, env)
        digraph.getClassRepr(tpt.tpe) match {
          case Some(clazz) =>
            if (hasCT(vs)) {
              val asymb = cnstrct.symbol
              val meth = clazz.getMemberTree(asymb.name, asymb.tpe)
              val memc = getSpecializedClass(clazz.tree.symbol.name.toTypeName, tpt.tpe,
                meth.vparamss.flatten, vs)
              val rargs = getRuntimeArgs(trees, vs)
              val rparams = getRuntimeParams(meth.vparamss.flatten, vs)
              val newnw = mkConstructorCall(Ident(memc.symbol), rargs)
              val env3 = Environment(rparams.map(_.symbol) zip vs.filter(isNotCT(_)))
              val nobj = HPEObject(newnw, clazz.tpe, env3)
              (newnw, AbsValue(nobj), env)
            } else {
              val store = Environment.empty
              val cnstrct2 = mkApply(n, trees)
              val obj = HPEObject(cnstrct2, tpt.tpe, store)
              (cnstrct2, AbsValue(obj), env2)
            }
          case None =>
            val cnstrct2 = mkApply(n, trees)
            (cnstrct2, Top, env2)
        }
      case lbl @ LabelDef(name, params, rhs) =>
        val (r, _, env2) = peval(rhs, env)
        val lbl2 = mkLabel(name, params, r)
        (lbl2, Top, env2)
      // Unary operations
      case select @ Select(qual, name) if (isUnary(select)) =>
        val (r1, v1, env1) = peval(qual, env)
        v1 match {
          case x: CTValue =>
            val (r, env2) = doUnaryOperation(name, x, env)
            (r.toTree, r, env2)
          case x: AbsValue =>
            val (r, env2) = doUnaryOperation(name, x.toCTValue, env)
            (r.toTree, r, env2)
          case _ =>
            val r = mkSelect(r1, name)
            (r, Top, env1)
        }
      case select @ Select(qual, name) =>
        val (r1, v, env1) = peval(qual, env)
        if (hasSymbol(qual) && hasSymbol(select)) {
          digraph.getClassRepr(select.symbol.owner.tpe) match {
            case Some(repr) =>
              val member = repr.getMemberTree(name, select.symbol.tpe)
              val (res, vl, envr) = v.value match {
                case Some(x @ HPEObject(tree, _, store)) =>
                  val (tt, vv, ss) = peval(member, store)
                  (tt, vv, env1.addValue(qual.symbol, vv))
                case _ =>
                  peval(member, env1)
              }
              vl match {
                case x: CTValue =>
                  (res, vl, envr)
                case _ =>
                  // TODO: Why this does not work??
                  // val ts = mkSelect(r1, name)
                  val ts = treeCopy.Select(select, r1, name)
                  (ts, Top, envr)
              }
            case None =>
              (select, Top, env1)
          }
        } else (select, Top, env1)
      case a @ Apply(fun, t) if (fun.symbol.name == newTermName("CT")) =>
        val (v, env2) = feval(t.head, env)
        (v.value, v, env2)
      case a @ Apply(fun, t) if (fun.symbol.name == newTermName("RT")) =>
        t match {
          case x :: Nil =>
            x.foreach {
              _ match {
                case v: Ident =>
                  val value = env.getValue(v.symbol)
                  value match {
                    case CTValue(_) => fail("CT cannot live inside RT")
                    case _ =>
                  }
                case _ =>
              }
            }
            peval(x, env)
          case _ => fail("Should not happen")
        }
      case apply @ Apply(fun @ Select(r, l), args) if (isBinary(apply)) =>
        val arg1 = apply.args.head
        val (r1, v1, env1) = peval(r, env)
        val (arg2, v2, env2) = peval(arg1, env1)
        val method = fun.symbol
        val methodName = method.name
        (v1, v2) match {
          case (x: CTValue, y: CTValue) =>
            val (r, env3) = doBinaryOperation(methodName, x, y, env)
            (r.toTree, r, env3)
          case (x: CTValue, y: AbsValue) =>
            val (r, env3) = doBinaryOperation(methodName, x, y.toCTValue, env)
            (r.toTree, r, env3)
          case (x: AbsValue, y: CTValue) =>
            val (r, env3) = doBinaryOperation(methodName, x.toCTValue, y, env)
            (r.toTree, r, env3)
          case (x: AbsValue, y: AbsValue) =>
            val (r, env3) = doBinaryOperation(methodName, x.toCTValue, y.toCTValue, env)
            (r.toTree, r, env3)
          case _ =>
            // TODO: Fix this
            val rr = treeCopy.Select(fun, r1, l)
            val r = typed(treeCopy.Apply(apply, rr, List(arg2)))
            (r, Top, env2)
            // val rr = mkSelect(r1, l)
            // val r = mkApply(rr, List(arg2))
            // (r, Top, env2)
        }
       case apply @ Apply(fun @ Select(qual @ Super(k, j), m), args) =>
        //          val tpe = qual.symbol.tpe
        //          digraph.getClassRepr(tpe) match {
        //            case Some(clazz) =>
        //              val rcvr = clazz.tree
        //              pevalApply(rcvr, Top, tpe, fun.symbol.name, apply, fun,
        //                (tmpt: Tree, newt: Tree, x: Name) => 
        //                  treeCopy.Select(tmpt, qual, x),
        //                  	args, env)
        //            case None => noTreeApply(apply, env) 
        //          }
        //TODO Think about this case more
        // Do we know who is our parent, during compilation time?
        // super.find() which find is called?
        (apply, Top, env)
      case apply @ Apply(fun, args) =>
        val tpe = fun.symbol.owner.tpe
        digraph.getClassRepr(tpe) match {
          case Some(clazz) =>
            val (obj, nv, env2) = fun match {
              case Select(This(c), m) => (This(c), Top, env)
              case Select(qual, m) => 
                val (rr, vv, ss) = peval(qual, env)
                vv.value match {
                  case Some(x: HPEObject) => (rr, vv, x.store)
                  case _ => (rr, vv, ss)
                }
              case m => peval(m, env)
            }
            val (rr, vv, ss) = pevalApply(obj, nv, tpe, fun.symbol.name, apply, fun,
              args, env2, env)
           fun match {
              case Select(This(c), m) => (rr, vv, ss)
              case Select(qual, m) =>
                vv.value match {
                  case Some(x: HPEObject) =>
                    if(vv.isCT) {
                      val nbj = CTValue(x.copy(store = ss))
                      (rr, vv, env.addValue(fun.symbol, nbj))
                    }
                    else (rr, vv, ss) 
                  case _ => (rr, vv, ss)
                }
              case _ => (rr, vv, ss)
            }
          case None => noTreeApply(apply, env)
        }
      case rtrn @ Return(t) =>
        val (r, v, env2) = peval(t, env)
        val rtrn2 = mkReturn(r)
        (rtrn2, v, env2)
      case Typed(exp, t2) => peval(exp, env)
      case _ => (tree, Top, env)
    }
  }

  private def noTreeApply(apply: Apply, env: Environment): 
                (Tree, Value, Environment) = {
    val tpe = apply.fun.symbol.owner.tpe
    val (pargs, vs, env2) = pevalArgs(apply.args, env)
    vs.filter(isCT(_)) match {
      case Nil =>
        val app = mkApply(apply.fun, pargs)
        (app, Top, env2)
      case _ =>
        fail(s"${vs}\nTree not found for ${tpe} the " +
          s"owner of ${apply.fun.symbol.name} " +
          s"and the call is: ${apply}")
    }
  }


  def isCT(v: Value) = !isNotCT(v)


  def isNotCT(v: Value) = {
    v match {
      case x: CTValue => false
      case _ => true
    }
  }

  private def getRuntimeArgs(exprs: List[Tree], 
        values: List[Value]): List[Tree] = {

    val pvTuple = values zip exprs
    val temp = for ((v, e) <- pvTuple if (isNotCT(v))) yield {
      e
    }
    temp.reverse
  }

  private def getCTArgs(exprs: List[ValDef], 
              values: List[Value]): List[ValDef] = {

    val pvTuple = values zip exprs
    val temp = for ((v, e) <- pvTuple if (isCT(v))) yield {
      e
    }
    temp.reverse
  }

  private def getRuntimeParams(params: List[Tree], 
                  vals: List[Value]): List[ValDef] = {

    val rparams = for ((param, v) <- (params zip vals) if (isNotCT(v))) yield {
      param.asInstanceOf[ValDef]
    }
    rparams.reverse
  }

  private def getSpecializedMethod(clazz: ClassRepr,
    method: DefDef, args: List[Value],
    ctnames: List[Name], 
    env: Environment, 
    ret: Type): (DefDef, Environment) = {
    clazz.getSpecializedOption(method.symbol.name, ctnames, args) match {
      case Some(mtree) => (mtree, env)
      case None =>
        val clazzSymb = clazz.tree.symbol match {
          case x: Symbol if (x.isModule) => x.moduleClass
          case y => y
        }
        val ctvals = args.filter(isCT(_))
        val tmargs = method.vparamss.flatten
        val cargs = getCTArgs(tmargs, args)
        val rparams = getRuntimeParams(tmargs, args)
        val sname = clazz.getNextMethod(method.symbol.name, ctnames, ctvals)


        val newMethod = method.duplicate(sname, clazzSymb).removeParams(cargs, 
                    ctvals.map((x) => hpeAny2Tree(x.value)))
        val (pevaledMethod, _, _) = peval(newMethod, env)
        val temp = clazz.tree.addMember(pevaledMethod)


        clazz.addSpecialized(method.symbol.name, ctnames, args, pevaledMethod)
        clazz.tree = temp
        (pevaledMethod, env.remove(cargs.map(_.symbol)))

        // TODO: A good use-case for Lombrello, heavily refactorable





        // val symb = clazzSymb.newMethod(sname, clazzSymb.pos.focus, method.symbol.flags)
        // val obody = method.rhs.duplicate
        // val (mbody, _, _) = peval(obody, env)
        // val paramSyms = map2(rparams.map(_.symbol.tpe), rparams.map(_.symbol)) {
        //   (tp, param) => symb.newSyntheticValueParam(tp, param.name.toTermName)
        // }
        // val tpe = MethodType(paramSyms, ret)

        // val nrparams = for ((p, s) <- (rparams zip paramSyms)) yield {
        //   s.setInfo(p.symbol.tpe)
        //   ValDef(s, p.tpt)
        // }

        // if (clazzSymb.info.decls.lookup(symb.name) == NoSymbol) {
        //   symb setInfoAndEnter tpe
        // } else {
        //   symb setInfo tpe
        // }

        // fixOwner(mbody, method.symbol, symb, paramSyms)

        // val tbody = localTyper.typedPos(symb.pos)(mbody)

        // val methDef = DefDef(symb, List(nrparams), tbody)

        // methDef.tpt setType localTyper.packedType(tbody, symb)
        // val mtree = typed(methDef).asInstanceOf[DefDef]

        // val temp = clazz.tree match {
        //   case m: ModuleDef => typed {
        //     treeCopy.ModuleDef(m, m.mods,
        //       m.symbol.name,
        //       treeCopy.Template(m.impl,
        //         m.impl.parents,
        //         m.impl.self,
        //         mtree :: m.impl.body))
        //   }.asInstanceOf[ImplDef]
        //   case c: ClassDef => typed {
        //     treeCopy.ClassDef(c, c.mods,
        //       c.symbol.name, c.tparams,
        //       treeCopy.Template(c.impl,
        //         c.impl.parents,
        //         c.impl.self,
        //         mtree :: c.impl.body))
        //   }.asInstanceOf[ImplDef]
        // }

        // clazz.addSpecialized(method.symbol.name, ctnames, args, mtree)
        // clazz.tree = temp
        // (mtree, env.remove(cargs.map(_.symbol)))
    }
  }

  private def getSpecializedClass(name: TypeName, tpe: Type,
    args: List[ValDef], vals: List[Value]): ClassDef = {
    val ctargs = getCTArgs(args, vals)
    val ctvals = vals.filter(isCT(_))
    val ctnames = ctargs.map(_.symbol.name)
    classBank.getOption(name, ctnames, ctvals) match {
      case Some(x) => x.tree.asInstanceOf[ClassDef]
      case None =>
        val clazz = digraph.getClassRepr(tpe).get
        val tpes = args.map(_.symbol)
        clazz.getMemberTree(nme.CONSTRUCTOR, MethodType(tpes, tpe)) match {
          case mtree: DefDef =>
            val classSymb = clazz.tree.symbol
            val newName = classBank.getNextClassName(name, ctnames, ctvals)
            val clazzArgs = getRuntimeParams(args, vals)



            val duplicated = clazz.tree.duplicate(newName).removeMember((x) => 
              {
                isConstructor(x) || isVal(x) || isVar(x) || 
                          x.symbol.isSetter || x.symbol.isGetter
              })

            val clazzL = 
              duplicated.updateParents(
                List(clazz.tree.symbol.toType)).asInstanceOf[ClassDef]


            val const = clazzL.mkConstructor


            val nConst = clazzArgs.foldLeft(const)((z, y) => {
                addConstructorParam(y.symbol.name.toString, y.symbol.info, z)
            })

            val nimpl = clazzL.addMember(nConst.addSuperConstructorCall)



            // TODO: A good use-case for Lombrello, heavily refactorable
            // val otemp = clazz.tree.impl.duplicate
            // val omembers = otemp.body.filter(_.symbol.name != nme.CONSTRUCTOR)
            // val nparents = List(classSymb.tpe) 
            // val nsymb = classSymb.owner.newClass(newName, 
            //           classSymb.pos.focus, classSymb.flags)

            // val ntpe = ClassInfoType(nparents, newScope, nsymb)
            // nsymb.setInfoAndEnter(ntpe)
            // // TODO: This line does not work any more Scala 2.11
            // // nsymb.setTypeSignature(ntpe)
            // omembers.foreach(fixOwner(_, classSymb, nsymb))

            // val spr = Select(Super(This(nsymb), tpnme.EMPTY), nme.CONSTRUCTOR)
            // val sapply = Apply(spr, Nil)

            // val constSymb = nsymb.newClassConstructor(nsymb.pos.focus)

            // val paramSyms = map2(
            //       clazzArgs.map(_.symbol.tpe), clazzArgs.map(_.symbol)) {
            //   (tp, param) => 
            //     constSymb.newSyntheticValueParam(tp, param.name.toTermName)
            // }
            // val nrparams = for ((p, s) <- (clazzArgs zip paramSyms)) yield {
            //   s.setInfo(p.symbol.tpe)
            //   ValDef(s, p.tpt)
            // }

            // val nctargs = for (ct <- ctargs) yield {
            //   val s = nsymb.newValue(newTermName(ct.name + " "), 
            //           nsymb.pos.focus, ct.symbol.flags)
            //   s.setInfo(ct.symbol.tpe)
            //   ValDef(s, ct.tpt)
            // }
            // val env = Environment(nctargs.map(_.symbol) zip ctvals)

            // val constType = MethodType(paramSyms, nsymb.tpe)
            // constSymb.setInfoAndEnter(constType)

            // val ocbody = mtree.rhs.duplicate match {
            //   case block @ Block(stats, ret) =>
            //     stats match {
            //       case x :: xs =>
            //         Block(sapply :: xs, ret)
            //       case Nil =>
            //         Block(List(sapply), ret)

            //     }
            //   case x => x
            // }

            // val allsyms = paramSyms ++ nctargs.map(_.symbol)
            // fixOwner(ocbody, mtree.symbol, constSymb, allsyms)
            // // fixOwner(ocbody, classSymb, nsymb)



            // val ths = This(nsymb)
            // ths.setType(ntpe)

            // val nomembers = omembers.map(_.substituteThis(classSymb, ths))

            // nomembers.foreach {
            //   (x: Tree) =>
            //     {
            //       fixOwner(x, classSymb, nsymb, allsyms)
            //     }
            // }
            // val const = DefDef(constSymb, List(nrparams), ocbody)

            // var nimpl = typed(ClassDef(nsymb,
            //   Template(otemp.parents, otemp.self, 
            //         const :: nomembers))).asInstanceOf[ClassDef]

            // val nbody = {
            //   var tail = const :: nomembers
            //   var accum: List[Tree] = Nil
            //   var tenv = env
            //   while (tail != Nil) {
            //     val head = tail.head
            //     val sym = head.symbol
            //     if ((!sym.isMethod) && (sym.isValue || sym.isVariable)) {
            //       nctargs.filter(_.symbol.name.toString == sym.name.toString) match {
            //         case param :: _ =>
            //           accum
            //         case Nil => accum = head :: accum
            //       }
            //     } else {
            //       val (t, _, temp) = peval(head, tenv)
            //       tenv = temp
            //       accum = t :: accum
            //     }
            //     tail = tail.tail
            //   }
            //   accum
            // }

            // nimpl = typed(ClassDef(nsymb,
            //   Template(otemp.parents, otemp.self, nbody))).asInstanceOf[ClassDef]





            val clazzrepr = new ClassRepr(nimpl.symbol.tpe, nimpl)
            digraph.addClass(clazzrepr)
            classBank.add(name, tpe, ctnames, ctvals, clazzrepr)
            clazzrepr.tree = nimpl
            clazzrepr.isSpecialized = true
            digraph.addSubclass(clazz, clazzrepr)
            classBank.add(name, tpe, ctnames, vals, clazzrepr)
            nimpl
          case _ => fail("Could not find the AST node of the constructor")
        }
    }
  }

  private def hasCT(vs: List[Value]): Boolean = {
    vs match {
      case Nil => false
      case CTValue(_) :: xs => true
      case x :: xs => hasCT(xs)
    }
  }
  private def isAllCT(vs: List[Value]): Boolean = {
    def check(args: List[Value]): Boolean = {
      args match {
        case Nil => true
        case CTValue(_) :: xs => check(xs)
        case _ => false
      }
    }
    if (vs == Nil) false
    else check(vs)
  }
  private def pevalArgs(args: List[Tree], 
        store: Environment): (List[Tree], List[Value], Environment) = {
    var pevaled: List[Value] = Nil
    var trees: List[Tree] = Nil
    var env = store
    var tail = args
    while (tail != Nil) {
      val head = tail.head
      val (t, v, temp) = peval(head, env)
      pevaled = v :: pevaled
      trees = t :: trees
      env = temp
      tail = tail.tail
    }
    (trees.reverse, pevaled.reverse, env)
  }
  private def fevalArgs(args: List[Tree], 
        store: Environment): (List[CTValue], Environment) = {
    var fevaled: List[CTValue] = Nil
    var env = store
    var tail = args
    while (tail != Nil) {
      val head = tail.head
      val (arg, temp) = feval(head, env)
      fevaled = arg :: fevaled
      env = temp
      tail = tail.tail
    }
    (fevaled.reverse, env)
  }

  private def fail(msg: String): Nothing = {
    throw new HPEError(msg)
  }

  private implicit def zip2Lists(list: (List[TermName], 
          List[Value])): List[(TermName, Value)] = {
    list._1 zip list._2
  }

  private def pevalApply(rcvr: Tree,
    nv: Value, rcvrtpe: Type,
    m: Name, apply: Apply, fun: Tree,
    args: List[Tree], store: Environment, 
            cntxt: Environment): (Tree, Value, Environment) = {
    val (pargs, pvals, env3) = pevalArgs(args, cntxt)
    digraph.getClassRepr(rcvrtpe) match {
      case Some(rcvclass) =>

        // TODO: A good use-case for Lombrello, heavily refactorable
        val mtree = rcvclass.getMemberTree(m, apply.symbol.tpe).asInstanceOf[DefDef]
        val tmargs = mtree.vparamss.flatten
        val cargs = getCTArgs(tmargs, pvals)
        val margs = cargs.map(_.symbol)
        val ctvals = pvals.filter(isCT(_))
        val menv = store.addValues(margs zip ctvals)
        val ctnames = cargs.map(_.symbol.name)
        nv match {
          case x: CTValue =>
            if (isAllCT(pvals)) {
              val (r, menv2) = feval(mtree.rhs, menv)
              (r.v.tree, r, store)
            } else {
              val rargs = getRuntimeArgs(args, pvals)
              val rparams = getRuntimeParams(tmargs, pvals)

              val module = getCompanionObject(rcvclass)

              val (specialized, renv) = getSpecializedMethod(module, mtree, 
                      pvals, ctnames, menv, apply.tpe)
              val sapply = mkApply(Select(Ident(module.tree.symbol), 
                                  specialized.symbol.name), 
                                  rargs)//.setSymbol(specialized.symbol)
              (typed(sapply), Top, env3)
            }
          case x: AbsValue =>
            if (hasCT(pvals)) {
              val rargs = getRuntimeArgs(args, pvals)
              val mname = rcvclass.getNextMethod(mtree.symbol.name, 
                        ctnames, ctvals)
              val (specialized, envr) = getSpecializedMethod(rcvclass, mtree, 
                        pvals, ctnames, menv, apply.tpe)
              val sapply = mkApply(Select(rcvr, specialized.symbol.name), 
                                  rargs)//.setSymbol(specialized.symbol)
              val tapply = typed(sapply)
              (tapply, Top, envr)
            } else {

              val tapply = typed(treeCopy.Apply(apply,
                apply.fun, pargs))
              (tapply, Top, env3)
            }
          case _ => // receiver is unknown
            if (hasCT(pvals) && closed) {
              val rargs = getRuntimeArgs(args, pvals)
              val clazzes = rcvclass :: digraph.getSubclasses(rcvclass)
              val mname = rcvclass.getNextMethod(mtree.symbol.name, 
                        ctnames, ctvals)
              for (c <- clazzes) {
                val (specialized, envr) = getSpecializedMethod(c, mtree, pvals, 
                        ctnames, menv, apply.tpe)
              }
              val mthd = rcvclass.getSpecialized(mname, ctnames, pvals)
              val tapply = typed(mkApply(Select(rcvr, mname), 
                          rargs))//.setSymbol(mthd.symbol))
              (tapply, Top, env3)
            } else {
              val tapply = typed(treeCopy.Apply(apply, fun, pargs))
              (tapply, Top, env3)
            }
        }
      case _ =>
        val tapply = typed(treeCopy.Apply(apply, fun, pargs))
        (tapply, Top, env3)
    }
  }

  private def fevalApply(reciever: Type, method: Symbol,
    args: List[Tree], cntxt: Environment,
    store: Environment): (CTValue, Environment, Environment) = {
    digraph.getClassRepr(reciever) match {
      case Some(clazz) =>
        val mtree = tree2Method(clazz.getMemberTree(method.name, method.tpe))
        val (fevaledArgs, env1) = fevalArgs(args, store)
        val params = mtree.vparamss.flatten.map(_.symbol)
        val funStore = store.addValues(params zip fevaledArgs)
        val (v, store2) = feval(mtree.rhs, funStore)
        store2.remove(params)
        (v, env1, store2)
      case None =>
        fail(fevalError)
    }
  }

  private def getCompanionObject(rcvclass: ClassRepr): ClassRepr = {

    // TODO: A good use-case for Lombrello, heavily refactorable
    digraph.findCompanionModule(rcvclass.tree.symbol) match {
      case None =>
        val clazz = rcvclass.tree.asInstanceOf[ClassDef]


        val module = clazz.mkCompanionObject
        // val csymbol = rcvclass.tree.symbol.asInstanceOf[ClassSymbol]

        // val modname = csymbol.name
        // val owner = csymbol.owner
        // val symb = csymbol.newModule(modname.toTermName,
        //   csymbol.pos.focus, MODULE)
        // val msymb = symb.moduleClass
        // symb.owner = owner
        // msymb.owner = owner
        // val parents = rcvclass.tree.impl.parents
        // val tparents = parents.map(_.tpe)

        // val mtpe = ClassInfoType(tparents, newScope, msymb)

        // msymb setInfo mtpe
        // symb setInfoAndEnter msymb.tpe

        // val csymb = msymb.newClassConstructor(symb.pos.focus)
        // csymb setInfoAndEnter (MethodType(Nil, symb.info))

        // val spr = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
        // val sapply = Apply(spr, Nil)
        // val cnsrct = DefDef(csymb, List(Nil),
        //   Block(List(sapply), Literal(Constant(()))))

        // val module = ModuleDef(symb, Template(parents,
        //   noSelfType,
        //   List(cnsrct)))

        // val tmodule = typed(module).asInstanceOf[ModuleDef]
        val modrepr = new ClassRepr(module.symbol.tpe, module)
        digraph.addClass(modrepr)
        digraph.addCompanion(rcvclass.tree.tpe, modrepr)
        modrepr
      case Some(x) => x
    }
  }

  private implicit def hpeAny2Tree(t: Option[HPEAny]): Tree = {
    t match {
      case Some(HPELiteral(x: Tree, _)) => x
      case Some(HPEObject(x: Tree, _, _)) => x
      case Some(HPETree(t)) => t
      case _ =>
        typed(treeBuilder.makeBlock(Nil))
    }
  }

  private implicit def hpeAny2Type(t: Option[HPEAny]): Type = {
    t match {
      case Some(HPELiteral(_, x: Type)) => x
      case Some(HPEObject(_, x: Type, _)) => x
      case _ => fail(s"Unexpected type definition ${t}")
    }
  }

  private implicit def tree2Literal(t: Tree): Literal = {
    t match {
      case x: Literal => x
      case _ => fail(s"${t} is not a Literal")
    }
  }

  private implicit def tree2Field(t: Tree): ValDef = {
    t match {
      case x: ValDef => x
      case x => fail(s"Unexpected val definition ${x}")
    }
  }

  private implicit def tree2Class(t: Tree): ImplDef = {
    t match {
      case x: ImplDef => x
      case x => fail(s"Unexpected class definition ${x} ${x.symbol}")
    }
  }

  private implicit def tree2Method(t: Tree): DefDef = {
    t match {
      case x: DefDef => x
      case x => fail(s"Unexpected method definition ${x}")
    }
  }
  private def getMemberTree(tpe1: Type, m: Name, mType: Type): Tree = {
    digraph.getClassRepr(tpe1) match {
      case Some(repr) =>
        repr.getMemberTree(m, mType)
      case None => fail(s"Could not find class ${tpe1}")
    }
  }


  private def doUnaryOperation(methodName: Name, 
            v: CTValue, env: Environment): (CTValue, Environment) = {
    v.value match {
      case Some(HPELiteral(x, _)) =>
        val lit = doUop(x, methodName)
        val tlit = typed(lit)
        val r = CTValue(HPELiteral(tlit, tlit.tpe))
        (r, env)
      case _ => fail(fevalError)
    }
  }

  private def doBinaryOperation(methodName: Name, v1: CTValue,
    v2: CTValue, env: Environment): (CTValue, Environment) = {
    (v1.value, v2.value) match {
      case (Some(HPELiteral(x, _)), Some(HPELiteral(y, _))) =>
        val lit = doBop(x, y, methodName)
        val tlit = typed(lit)
        val r = CTValue(HPELiteral(tlit, tlit.tpe))
        (r, env)
      case _ => fail(s"${fevalError} BOP ${v1.value} and ${v2.value}")
    }
  }
}
