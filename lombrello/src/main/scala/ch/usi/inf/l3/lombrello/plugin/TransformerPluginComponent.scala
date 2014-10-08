/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.plugin

import transformers._
import ch.usi.inf.l3.lombrello.util._
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

abstract class TransformerPluginComponent(val plgn: LombrelloPlugin)
  extends PluginComponent
  with LombrelloImplicitsCake
  with Transform
  with TypingTransformers
  with TreeDSL
  with RenameTransformerCake 
  with TreeGenTransformerCake 
  with TreeDuplicatorCake 
  with TreeModifiersCake 
  with TreeTraversersCake 
  with ExtractorTransformerCake
  with TreeTransformersCake {

  import plgn._

  
  val global: plgn.global.type = plgn.global

  import plgn.global._


  def newTransformer(unit: CompilationUnit): Transformer 
  
  /**
    * The plugin framework should have a refactoring mode:
    *
    * 1- If you rename a field all its setters and getters (and of course uses
    *    shall be renamed).
    * 2- If you add/remove a param in a method or constructor, the framework
    *    should automatically pass defaults/drop the arg for that param.
    */
  abstract class TransformerComponent(val unit: CompilationUnit)
    extends TypingTransformer(unit)
    with LombrelloImplicits
    with RenameTransformer 
    with TreeDSL
    with TreeGenTransformer
    with TreeDuplicator
    with TreeModifiers
    with TreeTraversers 
    with ExtractorTransformer 
    with TreeTransformers {


    val global: plgn.global.type = plgn.global

    

    
    /**
      * Types a tree and returns the typed tree
      * 
      * @param tree the tree to be typed
      * 
      * @return a typed tree
      */
    protected def typed(tree: Tree): Tree = localTyper.typed(tree)


    /**
      * Returns a fresh and unique TermName based on a given TermName
      *
      * @param base the base TermName
      * 
      * @return a fresh and unique TermName
      */
    def freshName(base: TermName): TermName = {
      unit.freshTermName(base.toString)
    }
  }

}
