/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.plugin

import transformers._
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

abstract class InfoTransformerPluginComponent(override val plgn: LombrelloPlugin)
  extends TransformerPluginComponent(plgn)
  with InfoTransform {

  import plgn._

  
  override val global: plgn.global.type = plgn.global

  import plgn.global._


  def newTransformer(unit: CompilationUnit): Transformer 

  def transformInfo(sym: Symbol, tpe: Type): Type 

}
