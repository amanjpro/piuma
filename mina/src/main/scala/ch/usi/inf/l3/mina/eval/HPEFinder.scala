/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.mina.eval

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.language.implicitConversions
import ch.usi.inf.l3.mina._

@checker("mina-finder") class HPEFinder {

  plugin HPE

  rightAfter(bfr)
  before(List(specializer))


  def check(unit: CompilationUnit) {
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
