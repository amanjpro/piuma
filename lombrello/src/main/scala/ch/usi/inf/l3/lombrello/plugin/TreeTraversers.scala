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

    trait TreeTraverserPredicator extends Function1[Tree, Boolean] {

      def apply(x: Tree): Boolean = traverse(x)

      def shouldCheck(t: Tree): Boolean = true

      def otherwise(t: Tree): Boolean = true

      def traverse(trees: List[Tree]): Boolean = {
        trees.foldLeft(false)((z, y) => z || traverse(y))
      }

      def traverseAlternative(tree: Alternative): Boolean = {
        traverse(tree.trees)
      }

      def traverseAnnotated(tree: Annotated): Boolean = {
        traverse(tree.annot) || traverse(tree.arg)
      }


      def traverseAppliedTypeTree(tree: AppliedTypeTree): Boolean = {
        traverse(tree.tpt) || traverse(tree.args)
      }

      def traverseApply(tree: Apply): Boolean = {
        traverse(tree.fun) || traverse(tree.args)
      }

      def traverseAssign(tree: Assign): Boolean = {
        traverse(tree.lhs) || traverse(tree.lhs)
      }

      def traverseBind(tree: Bind): Boolean = {
        traverse(tree.body)
      }

      def traverseBlock(tree: Block): Boolean = {
        traverse(tree.stats) || traverse(tree.expr)
      }

      def traverseCaseDef(tree: CaseDef): Boolean = {
        traverse(tree.pat) || traverse(tree.guard) || traverse(tree.body)
      }

      def traverseClassDef(tree: ClassDef): Boolean = {
        traverse(tree.tparams) || traverse(tree.impl)
      }

      def traverseCompoundTypeTree(tree: CompoundTypeTree): Boolean = {
        traverse(tree.templ)
      }

      def traverseDefDef(tree: DefDef): Boolean = {
        traverse(tree.tparams) || 
          traverse(tree.vparamss.flatten) ||
          traverse(tree.tpt) || 
          traverse(tree.rhs)
      }

      def traverseExistentialTypeTree(tree: ExistentialTypeTree): Boolean = {
        traverse(tree.tpt) || traverse(tree.whereClauses)
      }

      def traverseFunction(tree: Function): Boolean = {
        traverse(tree.vparams) || traverse(tree.body)
      }

      def traverseIdent(tree: Ident): Boolean
      

      def traverseIf(tree: If): Boolean = {
        traverse(tree.cond) || traverse(tree.thenp) || traverse(tree.elsep)
      }

      def traverseImport(tree: Import): Boolean = {
        traverse(tree.expr)
      }

      def traverseLabelDef(tree: LabelDef): Boolean = {
        traverse(tree.params) || traverse(tree.rhs)
      }

      def traverseLiteral(tree: Literal): Boolean = {
        shouldCheck(tree) && otherwise(tree)
      }

      def traverseMatch(tree: Match): Boolean = {
        traverse(tree.selector) || traverse(tree.cases)
      }

      def traverseModuleDef(tree: ModuleDef): Boolean = {
        traverse(tree.impl)
      }

      def traverseNew(tree: New): Boolean = {
        traverse(tree.tpt)
      }

      def traversePackageDef(tree: PackageDef): Boolean = {
        traverse(tree.pid) || traverse(tree.stats)
      }

      def traverseRefTree(tree: RefTree): Boolean = {
        traverse(tree.qualifier)
      }

      def traverseReferenceToBoxed(tree: ReferenceToBoxed): Boolean = {
        traverse(tree.ident)
      }

      def traverseReturn(tree: Return): Boolean = {
        traverse(tree.expr)
      }

      def traverseSelect(tree: Select): Boolean = {
        traverse(tree.qualifier)
      }

      def traverseSelectFromTypeTree(tree: SelectFromTypeTree): Boolean = {
        traverse(tree.qualifier)
      }

      def traverseSingletonTypeTree(tree: SingletonTypeTree): Boolean = {
        traverse(tree.ref)
      }

      def traverseStar(tree: Star): Boolean = {
        traverse(tree.elem)
      }

      def traverseSuper(tree: Super): Boolean = {
        traverse(tree.qual)
      }

      def traverseTemplate(tree: Template): Boolean = {
        traverse(tree.parents) || traverse(tree.self) || traverse(tree.body)
      }

      def traverseThis(tree: This): Boolean = {
        shouldCheck(tree) && otherwise(tree)
      }

      def traverseThrow(tree: Throw): Boolean = {
        traverse(tree.expr)
      }

      def traverseTry(tree: Try): Boolean = {
        traverse(tree.block) || 
        traverse(tree.catches) || 
        traverse(tree.finalizer)
      }

      def traverseTypeApply(tree: TypeApply): Boolean = {
        traverse(tree.fun) || traverse(tree.args)
      }

      def traverseTypeBoundsTree(tree: TypeBoundsTree): Boolean = {
        traverse(tree.lo) || traverse(tree.hi)
      }

      def traverseTypeDef(tree: TypeDef): Boolean = {
        traverse(tree.tparams) || traverse(tree.rhs)
      }

      def traverseTypeTree(tree: TypeTree): Boolean = {
        shouldCheck(tree) && otherwise(tree)
      }

      def traverseTyped(tree: Typed): Boolean = {
        traverse(tree.expr) || traverse(tree.tpt)
      }

      def traverseUnApply(tree: UnApply): Boolean = {
        traverse(tree.fun) || traverse(tree.args)
      }

      def traverseValDef(tree: ValDef): Boolean = {
        traverse(tree.tpt) || traverse(tree.rhs)
      }

      /**
        * Traverses the tree and filters according to the predicate
        */
      def traverse(tree: Tree): Boolean = {
        tree match {
          case t: Alternative if shouldCheck(t) =>
            traverseAlternative(t)
          case t: Annotated if shouldCheck(t) =>
            traverseAnnotated(t)
          case t: AppliedTypeTree if shouldCheck(t) =>
            traverseAppliedTypeTree(t)
          case t: Apply if shouldCheck(t) =>
            traverseApply(t)
          case t: Assign if shouldCheck(t) =>
            traverseAssign(t)
          case t: Bind if shouldCheck(t) =>
            traverseBind(t)
          case t: Block if shouldCheck(t) =>
            traverseBlock(t)
          case t: CaseDef if shouldCheck(t) =>
            traverseCaseDef(t)
          case t: ClassDef if shouldCheck(t) =>
            traverseClassDef(t)
          case t: CompoundTypeTree if shouldCheck(t) =>
            traverseCompoundTypeTree(t)
          case t: DefDef if shouldCheck(t) =>
            traverseDefDef(t)
          case t: ExistentialTypeTree if shouldCheck(t) =>
            traverseExistentialTypeTree(t)
          case t: Function if shouldCheck(t) =>
            traverseFunction(t)
          case t: Ident if shouldCheck(t) =>
            traverseIdent(t)
          case t: If if shouldCheck(t) =>
            traverseIf(t)
          case t: Import if shouldCheck(t) =>
            traverseImport(t)
          case t: LabelDef if shouldCheck(t) =>
            traverseLabelDef(t)
          case t: Literal if shouldCheck(t) =>
            traverseLiteral(t)
          case t: Match if shouldCheck(t) =>
            traverseMatch(t)
          case t: ModuleDef if shouldCheck(t) =>
            traverseModuleDef(t)
          case t: New if shouldCheck(t) =>
            traverseNew(t)
          case t: PackageDef if shouldCheck(t) =>
            traversePackageDef(t)
          case t: RefTree if shouldCheck(t) =>
            traverseRefTree(t)
          case t: ReferenceToBoxed if shouldCheck(t) =>
            traverseReferenceToBoxed(t)
          case t: Return if shouldCheck(t) =>
            traverseReturn(t)
          case t: Select if shouldCheck(t) =>
            traverseSelect(t)
          case t: SelectFromTypeTree if shouldCheck(t) =>
            traverseSelectFromTypeTree(t)
          case t: SingletonTypeTree if shouldCheck(t) =>
            traverseSingletonTypeTree(t)
          case t: Star if shouldCheck(t) =>
            traverseStar(t)
          case t: Super if shouldCheck(t) =>
            traverseSuper(t)
          case t: Template if shouldCheck(t) =>
            traverseTemplate(t)
          case t: This if shouldCheck(t) =>
            traverseThis(t)
          case t: Throw if shouldCheck(t) =>
            traverseThrow(t)
          case t: Try if shouldCheck(t) =>
            traverseTry(t)
          case t: TypeApply if shouldCheck(t) =>
            traverseTypeApply(t)
          case t: TypeBoundsTree if shouldCheck(t) =>
            traverseTypeBoundsTree(t)
          case t: TypeDef if shouldCheck(t) =>
            traverseTypeDef(t)
          case t: TypeTree if shouldCheck(t) =>
            traverseTypeTree(t)
          case t: Typed if shouldCheck(t) =>
            traverseTyped(t)
          case t: UnApply if shouldCheck(t) =>
            traverseUnApply(t)
          case t: ValDef if shouldCheck(t) =>
            traverseValDef(t)
          case t => otherwise(t)
        }
      }
    }
  }
}


// case t: Alternative(trees) =>
//   traverseAlternative(t)
// case t: Annotated(annot, arg) =>
//   traverseAnnotated(t)
// case t: AppliedTypeTree(tpt, args) =>
//   traverseAppliedTypeTree(t)
// case t: Apply(fun, args) =>
//   traverseApply(t)
// case t: Assign(lhs, rhs) =>
//   traverseAssign(t)
// case Bind(name, body) =>
//   traverseBind(t)
// case Block(stats, expr) =>
//   traverseBlock(t)
// case CaseDef(pat, guard, body) =>
//   traverseBlock(t)
// case ClassDef(mods, name, tparams, impl) =>
//   traverseBlock(t)
// case CompoundTypeTree(templ) =>
//   traverseBlock(t)
// case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
//   traverseBlock(t)
// case ExistentialTypeTree(tpt, whereClause) =>
//   traverseBlock(t)
// case Function(vparams, body) =>
//   traverseBlock(t)
// case Ident(qual, name) =>
//   traverseBlock(t)
// case If(cond, thenp, elsep) =>
//   traverseBlock(t)
// case Import(expr, selectors) =>
//   traverseBlock(t)
// case LabelDef(name, params, rhs) =>
//   traverseBlock(t)
// case Literal(value) =>
//   traverseBlock(t)
// case Match(selector, cases) =>
//   traverseBlock(t)
// case ModuleDef(mods, name, impl) =>
//   traverseBlock(t)
// case New(tpt) =>
//   traverseBlock(t)
// case PacakgeDef(pid, stats) =>
//   traverseBlock(t)
// case RefTree(qual, name) =>
//   traverseBlock(t)
// case ReferenceToBoxed(ident) =>
//   traverseBlock(t)
// case Return(expr) =>
//   traverseBlock(t)
// case Select(qual, name) =>
//   traverseBlock(t)
// case SelectFromTypeTree(qualifier, name) =>
//   traverseBlock(t)
// case SingletonTypeTree(ref) =>
//   traverseBlock(t)
// case Star(elem) =>
//   traverseBlock(t)
// case Super(qual, mix) =>
//   traverseBlock(t)
// case Template(parents, self, body) =>
//   traverseBlock(t)
// case This(qual) =>
//   traverseBlock(t)
// case Throw(expr) =>
//   traverseBlock(t)
// case Try(blcok, catches, finalizer) =>
//   traverseBlock(t)
// case TypeApply(fun, args) =>
//   traverseBlock(t)
// case TypeBoudnsTree(lo, hi) =>
//   traverseBlock(t)
// case TypeDef(mods, name, tparams, rhs) =>
//   traverseBlock(t)
// case x: TypeTree =>
//   traverseBlock(t)
// case Typed(expr, tpt) =>
//   traverseBlock(t)
// case UnApply(fun, args) =>
//   traverseBlock(t)
// case ValDef(mods, name, tpt, rhs) =>
//   traverseBlock(t)
//
