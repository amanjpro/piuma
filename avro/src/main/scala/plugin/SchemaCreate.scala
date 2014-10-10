package com.googlecode.avro
package plugin

// import scala.tools.nsc._
// import scala.tools.nsc.plugins.PluginComponent
//
import scala.collection.mutable.HashSet

import scala.reflect.internal.Flags._
import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import org.apache.avro.Schema



@checker("schemacreate") class SchemaCreate {
  // val global : ScalaAvroPlugin.this.global.type = ScalaAvroPlugin.this.global
  // val classToSchema = ScalaAvroPlugin.this.classToSchema
  // val unionToExtenders = ScalaAvroPlugin.this.unionToExtenders
  // val unionToSchemas = ScalaAvroPlugin.this.unionToSchemas
  // val unitMap = ScalaAvroPlugin.this.unitMap
  // val companionModuleMap = ScalaAvroPlugin.this.companionModuleMap
  // val companionClassMap = ScalaAvroPlugin.this.companionClassMap
// }
// trait SchemaCreate extends ScalaAvroPluginComponent {
//   import global._
//
  // val runsAfter = List[String]("unionclosure")
  rightAfter("unionclosure")
  plugin ScalaAvroPlugin
  // val phaseName = "schemacreate"

  // def newPhase(prev: Phase): Phase = new TraverserPhase(prev)
  // class TraverserPhase(prev: Phase) extends StdPhase(prev) {
    def check(unit: CompilationUnit) = {
      newTraverser().traverse(unit.body)
    }   
  // }

  private def newTraverser(): Traverser = new ForeachTreeTraverser(check1)

  private def check1(tree: Tree): Unit = tree match {
    case cd @ ClassDef(_, _, _, _) if (cd.symbol.tpe.parents.contains(avroRecordTrait.tpe)) =>
      val sym = cd.symbol
      val namespace = 
        if (sym.owner.fullName == "<empty>") None // Issue #6 - handle default package classes
        else Some(sym.owner.fullName)
      debug("Adding schema for class %s, namespace %s".format(sym.fullName, namespace))
      addRecordSchema(sym, 
          Schema.createRecord(sym.name.toString, "Auto-generated schema", namespace.orNull, false))
      debug("Registering class in companionClassMap")
      companionClassMap += sym.fullName -> sym
      debug("companionClassMap: " + companionClassMap) 
    case _ => ()
  }
}

