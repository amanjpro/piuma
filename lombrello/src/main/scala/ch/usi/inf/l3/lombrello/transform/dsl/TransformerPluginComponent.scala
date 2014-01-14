/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.transform.dsl

import transformers._
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

abstract class TransformerPluginComponent(val plugin: TransformerPlugin)
  extends PluginComponent
  with Transform
  with TypingTransformers
  with TreeDSL
  with RenameTransformerCake 
  with TreeGenTransformerCake 
  with TreeDuplicatorCake {

  import plugin._

  private var go_deeper = true
  
  
  def stop: Unit = go_deeper = false

  

  val global: plugin.global.type = plugin.global

  import global._

  def transform(cmpl: TransformerComponent, tree: Tree): Either[Tree, Tree]

  def newTransformer(unit: CompilationUnit): Transformer = new TransformerComponent(unit)
  
  /**
   * The plugin framework should have a refactoring mode:
   *
   * 1- If you rename a field all its setters and getters (and of course uses
   *    shall be renamed).
   * 2- If you add/remove a param in a method or constructor, the framework
   *    should automatically pass defaults/drop the arg for that param.
   */
  class TransformerComponent(val unit: CompilationUnit)
    extends TypingTransformer(unit)
    with RenameTransformer 
    with TreeGenTransformer
    with TreeDuplicator {

    val global: plugin.global.type = plugin.global
    
    protected def typed(tree: Tree): Tree = localTyper.typed(tree)

    final override def transform(tree: Tree): Tree = {
      val cntnu = TransformerPluginComponent.this.transform(this, tree)
      cntnu match {
        case Left(nTree) =>
        	super.transform(nTree)
        case Right(nTree) => nTree
      }
    }
  }

}