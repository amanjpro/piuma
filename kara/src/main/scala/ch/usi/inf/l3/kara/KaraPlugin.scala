/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.kara

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import ch.usi.inf.l3.kara.compiler.ReadDependantExtractor

@plugin(ReadDependantExtractor) class KaraPlugin {

  val beforeFinder = "typer" //FIXME: why this fails? utilities.PHASE_TYPER
  val name: String = "kara"
  describe( """A compiler plugin to add self-adjusted variables to Scala!""")
}
