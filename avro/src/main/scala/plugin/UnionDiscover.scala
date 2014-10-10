package com.googlecode.avro
package plugin

// import scala.tools.nsc._
// import scala.tools.nsc.plugins.PluginComponent

import scala.collection.mutable.HashSet
import scala.reflect.internal.Flags._

import org.apache.avro.Schema

import scala.collection.mutable.{HashMap,HashSet,MutableList,ListBuffer}
import ch.usi.inf.l3.lombrello.neve.NeveDSL._



@checker("uniondiscover") class UnionDiscover {
  // val global : ScalaAvroPlugin.this.global.type = ScalaAvroPlugin.this.global
  // val classToSchema = ScalaAvroPlugin.this.classToSchema
  // val unionToExtenders = ScalaAvroPlugin.this.unionToExtenders
  // val unionToSchemas = ScalaAvroPlugin.this.unionToSchemas
  // val unitMap = ScalaAvroPlugin.this.unitMap
  // val companionModuleMap = ScalaAvroPlugin.this.companionModuleMap
  // val companionClassMap = ScalaAvroPlugin.this.companionClassMap
// }
// trait UnionDiscover extends ScalaAvroPluginComponent {
//   import global._
  import global.definitions._

  // after(List("ctorretype"))
  rightAfter("ctorretype")
  plugin ScalaAvroPlugin
  // val phaseName = "uniondiscover"

  // def newPhase(prev: Phase): Phase = new TraverserPhase(prev)

  // class TraverserPhase(prev: Phase) extends StdPhase(prev) {
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


