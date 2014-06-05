/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.kara

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.lombrello.util._
import ch.usi.inf.l3.kara.compiler.typer.KaraTyper
import ch.usi.inf.l3.kara.compiler.ReadDependantExtractor

class KaraPlugin(override val global: TGlobal) extends TransformerPlugin(global) {

  val beforeFinder = utilities.PHASE_TYPER
  val name: String = "kara"
  override val description: String = """A compiler plugin to add self-adjusted variables to Scala!""";
  //                               ch.usi.inf.l3.kara.runtime.KaraVariable
  val karaClassName = "ch.usi.inf.l3.kara.runtime.KaraVariable"
  val karaRuntime = "ch.usi.inf.l3.kara.runtime.KaraRuntime"
  val karaAnnotationName = "ch.usi.inf.l3.kara.quals.incremental"
  val pluginComponents: List[TransformerPluginComponent] =
    List(
//        new KaraTyper(this), 
        new ReadDependantExtractor(this))
}
