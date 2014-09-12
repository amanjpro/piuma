/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.mina.eval

import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import ch.usi.inf.l3.mina._

class HPEFinder(val hpe: HPE) extends PluginComponent {

  import hpe._

  val global: hpe.global.type = hpe.global
  val runsAfter = List(bfr)
  override val runsRightAfter = Some(bfr)
  override val runsBefore = List[String](hpe.specializer)
  val phaseName = hpe.finder

  import hpe.global._
  def newPhase(_prev: Phase) = new HPEClassFinder(_prev)

  class HPEClassFinder(prev: Phase) extends StdPhase(prev) {
    override def name = hpe.finder

    def apply(unit: CompilationUnit) {
      for (tree <- unit.body) {
        tree match {
          case impl : ImplDef =>
            val repr = new ClassRepr(impl.symbol.tpe, impl)
            digraph.addClass(repr)
            for (p <- impl.impl.parents) {
              val parent = new ClassRepr(p.symbol.tpe)
              digraph.addClass(parent)
              digraph.addSubclass(parent, repr)
            }  
          case _ =>
        }

      }
    }
  }
}