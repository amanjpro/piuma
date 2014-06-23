/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.plugin

import transformers._
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

abstract class TransformerPluginComponent(val plgn: LombrelloPlugin)
  extends PluginComponent
  with Transform
  with TypingTransformers
  with TreeDSL
  with RenameTransformerCake 
  with TreeGenTransformerCake 
  with TreeDuplicatorCake 
  with TreeTraversersCake 
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
    with RenameTransformer 
    with TreeGenTransformer
    with TreeDuplicator
    with TreeTraversers 
    with TreeTransformers {

    val global: plgn.global.type = plgn.global

    
    protected def typed(tree: Tree): Tree = localTyper.typed(tree)

  }

}
