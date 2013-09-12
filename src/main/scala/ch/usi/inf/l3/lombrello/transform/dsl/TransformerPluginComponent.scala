/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.transform.dsl

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import ch.usi.inf.l3.lombrello.util.Utilities
import scala.reflect.macros.Macro



abstract class TransformerPluginComponent(val plugin: TransformerPlugin) 
		extends PluginComponent
		with Transform
		with TypingTransformers
		with TreeDSL 
		with Macro {

  import c.universe._
  import plugin._
  
  private var go_deeper = true
  
  
  def stop: Unit = go_deeper = false
  
  val utilities = Utilities
  
  val global: plugin.global.type = plugin.global

  import plugin.global._

  import plugin._
  
  def transform(tree: Tree): Tree
  
  def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
    override final def transform(tree: Tree): Tree = {
      go_deeper match {
        case true =>
          TransformerPluginComponent.this.transform(tree)
          super.transform(tree)
        case false =>
          TransformerPluginComponent.this.transform(tree)
      } 
    }
  }
}