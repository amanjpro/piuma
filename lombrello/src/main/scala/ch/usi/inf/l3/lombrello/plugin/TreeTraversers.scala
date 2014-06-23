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
