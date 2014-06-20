/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags
import scala.annotation.tailrec

// This trait is heavily inspired by (or stolen? from) 
// Tranformations trait in scala-refactoring

trait TreeTransformersCake {
  outer: TransformerPluginComponent =>
  trait TreeTransformers {
    self: outer.TransformerComponent =>

    import outer.plgn._
    import outer.plgn.global._


    // TODO: Fix the copyright thing -- Amanj

    /*
     * Copyright 2005-2010 LAMP/EPFL
     */


    /**
     * Transformations is the basis for all refactoring transformations.
     *
     * A transformation is a Function from X => Option[X], and can be
     * combined with other transformations in two ways:
     *   andThen - which applies the second transformation only if the
     *             first one was successful, i.e. returned Some(_).
     *   orElse  - which is applied only when the first transformation
     *             returned None.
     *
     * Xs are typically instances of global.Tree, but this is not
     * enforced. Once a transformations is assembled, it has to be applied
     * to a tree and its children. The function `all` applies a transformation
     * to the children of a tree. In the case of the trees, the tree has to
     * apply the transformation to all children and return one single tree.
     *
     * Additional functions are provided that apply a transformation top-down
     * or bottom-up.
     */
    trait Transformations {


      abstract class Transformation[X, Y] extends (X => Option[Y]) {

        self =>

        def apply(x: X): Option[Y]

        def andThen[Z](t: => Transformation[Y, Z]): Transformation[X, Z] = 
          new Transformation[X, Z] {
            def apply(x: X): Option[Z] = {
              self(x) flatMap t
            }
          }
        def &>[Z](t: => Transformation[Y, Z]): Transformation[X, Z] = andThen(t)

        def orElse(t: => Transformation[X, Y]): Transformation[X, Y] = 
          new Transformation[X, Y] {
            def apply(x: X): Option[Y] = {
              self(x) orElse t(x)
            }
          }

        def |>(t: => Transformation[X, Y]): Transformation[X, Y] = orElse(t)
      }
    

      /**
       * Construct a transformation from a partial function; this is the
       * most commonly used way to create new transformations, for example
       * like:
       *
       *   val reverse_all_class_members = transformation[Tree, Tree] {
       *     case t: Template => t.copy(body = t.body.reverse)
       *   }
       */
      def transformation[X, Y](f: PartialFunction[X, Y]) = 
        new Transformation[X, Y] {
          def apply(x: X): Option[Y] = f lift x
        }

      /**
       * We often want to use transformations as predicates, which execute
       * the next transformations if the result is true. For example:
       *
       *   val tree_with_range_pos = filter[Tree] {
       *     case t: Tree => t.pos.isRange
       *   }
       *
       * We can then use the predicate like this:
       *   tree_with_range_pos andThen do_something_with_the_tree orElse nothing
       */
      def predicate[X](f: PartialFunction[X, Boolean]) = new Transformation[X, X] {
        def apply(t: X): Option[X] = if (f.isDefinedAt(t) && f(t)) Some(t) else None
      }

      def predicate[X](f: X => Boolean) = new Transformation[X, X] {
        def apply(t: X): Option[X] = if (f(t)) Some(t) else None
      }

      /**
       * Always succeeds and returns the input unchanged.
       */
      def succeed[X] = new Transformation[X, X] {
        def apply(in: X): Option[X] = Some(in)
      }

      def id[X] = succeed[X]

      /**
       * Always fails, independent of the input.
       */
      def fail[X] = new Transformation[X, X] {
        def apply(in: X): Option[X] = None
      }

      def not[X](t: => Transformation[X, X]) = new Transformation[X, X] {
        def apply(x: X) = if (t(x).isDefined) None else Some(x)
      }
      def !  [X](t: => Transformation[X, X]) = not(t)

      /**
       * Applies a transformation to all subtrees of a tree T,
       * returning a new tree,typically of the same kind as T.
       *
       * If the transformation fails on one child, abort and
       * fail the whole application.
       */
      def allChildren(t: => Transformation[Tree, Tree]): Transformation[Tree, Tree] = 
        new Transformation[Tree, Tree] {
          def apply(in: Tree): Option[Tree] = {
            Some(traverse(in, (child: Tree) => t(child) getOrElse (return None)))
          }
        }

      /**
       * Applies a transformation to all subtrees of a tree T,
       * returning a new tree,typically of the same kind as T.
       *
       * If the transformation fails on one child, apply the
       * identity transformation `id` and don't fail, unlike
       * `allChildren`.
       */
      def matchingChildren(t: Transformation[Tree, Tree]) = allChildren(t |> id[Tree])

      /**
       * Applies a transformation top-down, that is, it applies
       * the transformation to the tree T and then passes the
       * transformed T to all children. The consequence is that
       * the children "see" their new parent.
       */
      def topdown (t: => Transformation[Tree, Tree]): Transformation[Tree, Tree] = 
        t &> allChildren(topdown(t))
      def preorder(t: => Transformation[Tree, Tree]): Transformation[Tree, Tree] = 
        t &> allChildren(topdown(t))

      /**
       * Applies a transformation bottom-up, that is, it applies
       * the transformation to the children of the tree first and
       * then to their parent. The consequence is that the parent
       * "sees" its transformed children.
       */
      def bottomup (t: => Transformation[Tree, Tree]): Transformation[Tree, Tree] = 
        allChildren(bottomup(t)) &> t
      def postorder(t: => Transformation[Tree, Tree]): Transformation[Tree, Tree] = 
        allChildren(bottomup(t)) &> t

      // TODO: Can you make this method pure? -- Amanj
      /**
       * Do a transformation until it succeeded once, then just fail.
       *
       * Note that because of the statefulness of once, you need to
       * make sure that it isn't accidentally passed as a by-name
       * parameter to another transformation and instantiated multiple
       * times.
       */
      def once [X <: AnyRef](t: Transformation[X, X]): Transformation[X, X] = 
        new Transformation[X, X] {
          var alreadyMatched = false
          def apply(x: X): Option[X] = {
            if(alreadyMatched) return None
            t(x) map { res =>
              // returning the same reference
              // does not count as "matched"
              alreadyMatched = !(x eq res)
              res
            }
          }
        }

      def traverseAndTransformAll(t: => Transformation[Tree, Tree]): 
            Transformation[Tree, Tree] = t |> topdown(matchingChildren(t))

      /**
       * Creates a transformation that always returns the value x.
       */
      def constant(y: Tree) = transformation[Tree, Tree] {
        case _ => y
      }
    }
  }
}
