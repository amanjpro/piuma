/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.lombrello.plugin

// import transformers._
// import scala.tools.nsc.transform.TypingTransformers
// import scala.tools.nsc.ast.TreeDSL
// import scala.tools.nsc.transform.Transform
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Phase
// import scala.language.implicitConversions
// import scala.reflect.runtime.universe._
// import ch.usi.inf.l3.lombrello.util.SymbolsUtil

abstract class CheckerPluginComponent(val plgn: LombrelloPlugin)
  extends PluginComponent {

  import plgn._

  
  val global: plgn.global.type = plgn.global

  import plgn.global._


  def newPhase(_prev: Phase): StdPhase
  
  abstract class CheckerComponent(override val prev: Phase)
    extends StdPhase(prev) {

    val global: plgn.global.type = plgn.global
  }

}
