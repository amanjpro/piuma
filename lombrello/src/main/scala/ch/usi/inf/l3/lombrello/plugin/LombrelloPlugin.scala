/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.parser.TreeBuilder
import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import scala.tools.nsc.doc.model.TreeFactory
import ch.usi.inf.l3.lombrello.util.Utilities
import ch.usi.inf.l3.lombrello.util.SymbolsUtil
import ch.usi.inf.l3.lombrello.util.TypesUtil
import ch.usi.inf.l3.lombrello.util.TreesUtil
import ch.usi.inf.l3.lombrello.util.SymbolsUtil
import ch.usi.inf.l3.lombrello.util.StoreScopeCake
// import scala.tools.nsc.symtab._
import ch.usi.inf.l3.lombrello.util.ScopeCake

abstract class LombrelloPlugin(val global: Global)
  extends Plugin
  with SymbolsUtil 
  with TreesUtil
  with TypesUtil
  with StoreScopeCake
  with ScopeCake {

  import global._

  val utilities = Utilities

  val beforeFinder: String

  lazy val finder = new ClassFinderComponent(this) {
    val runsAfter = List(beforeFinder)
  }
  val name: String;

  val description: String = """A compiler plugin!"""

  val pluginComponents: List[PluginComponent]
  lazy final val components: List[PluginComponent] = finder :: pluginComponents

  private var treeBank = Map.empty[Symbol, Tree]
  
  
  def addClassTree(sym: Symbol, t: Tree) = {
    treeBank = treeBank + (sym -> t)
  }
  
  def getClassTree(sym: Symbol): Option[Tree] = {
    treeBank.get(sym)
  }


  def addAnnotationChecker(annotationChecker: AnnotationChecker): Unit = {
    global.addAnnotationChecker(annotationChecker)
  }
}
