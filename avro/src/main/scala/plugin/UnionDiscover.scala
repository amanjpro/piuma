package com.googlecode.avro
package plugin

import scala.collection.mutable.HashSet
import scala.reflect.internal.Flags._

import org.apache.avro.Schema

import scala.collection.mutable.{HashMap,HashSet,MutableList,ListBuffer}
import ch.usi.inf.l3.piuma.neve.NeveDSL._



@checker("uniondiscover") class UnionDiscover {
  import global.definitions._

  rightAfter("ctorretype")
  plugin ScalaAvroPlugin


  private var unit: CompilationUnit = _
  private def check(_unit: CompilationUnit) = {
    unit = _unit
    newTraverser().traverse(unit.body)
  }   

  private def newTraverser(): Traverser = new ForeachTreeTraverser(check1)

  private def check1(tree: Tree): Unit = tree match {
    case cd @ ClassDef(mods, _, _, _) if (cd.symbol.tpe.parents.contains(avroUnionTrait.tpe)) =>
      if (!mods.isSealed)
        throw new NonSealedClassException(cd.symbol.fullName)
      // TODO: what do we do if it's not abstract?
      debug("Adding union " + cd.symbol.fullName + "to unit mapping")
      addUnionToUnit(unit, cd.symbol)
    case _ => ()
  }
}


