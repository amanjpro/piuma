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
trait TreeTraversersCake {
  traverser: TransformerPluginComponent =>
  trait TreeTraversers {
    self: traverser.TransformerComponent =>

    import traverser.plgn._
    import traverser.plgn.global._

    /**
      * Traverses the tree and applies f
      */
     def traverse(tree: Tree, f: Tree => Tree): Tree = {
      tree match {
        // case a @ Alternative(trees) =>
          // val ftrees = trees.map(traverse(_, f))
          // val falter = f(a)
          // falter.copy(trees = ftrees)
        case _ =>
          tree
      } 
    }



    /**
      * Traverses the tree and filters according to the predicate
      */
    def traverse(x: Tree, f: Tree => Boolean, n: Int): Tree

    
  }

}

// case Alternative(trees) =>
// case Annotated(annot, arg) =>
// case AppliedTypeTree(tpt, args) =>
// case Apply(fun, args) =>
// case Assign(lhs, rhs) =>
// case Bind(name, body) =>
// case Block(stats, expr) =>
// case CaseDef(pat, guard, body) =>
// case ClassDef(mods, name, tparams, impl) =>
// case CompoundTypeTree(templ) =>
// case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
// case ExistentialTypeTree(tpt, whereClause) =>
// case Function(vparams, body) =>
// case Ident(qual, name) =>
// case If(cond, thenp, elsep) =>
// case Import(expr, selectors) =>
// case LabelDef(name, params, rhs) =>
// case Literal(value) =>
// case Match(selector, cases) =>
// case ModuleDef(mods, name, impl) =>
// case New(tpt) =>
// case PacakgeDef(pid, stats) =>
// case RefTree(qual, name) =>
// case ReferenceToBoxed(ident) =>
// case Return(expr) =>
// case Select(qual, name) =>
// case SelectFromTypeTree(qualifier, name) =>
// case SingletonTypeTree(ref) =>
// case Star(elem) =>
// case Super(qual, mix) =>
// case Template(parents, self, body) =>
// case This(qual) =>
// case Throw(expr) =>
// case Try(blcok, catches, finalizer) =>
// case TypeApply(fun, args) =>
// case TypeBoudnsTree(lo, hi) =>
// case TypeDef(mods, name, tparams, rhs) =>
// case x: TypeTree =>
// case Typed(expr, tpt) =>
// case UnApply(fun, args) =>
// case ValDef(mods, name, tpt, rhs) =>
//
//
