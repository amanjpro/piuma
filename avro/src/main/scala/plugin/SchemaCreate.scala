package com.googlecode.avro
package plugin

import scala.collection.mutable.HashSet

import scala.reflect.internal.Flags._
import ch.usi.inf.l3.piuma.neve.NeveDSL._
import org.apache.avro.Schema



@checker("schemacreate") class SchemaCreate {
  rightAfter("unionclosure")
  plugin ScalaAvroPlugin

  def check(unit: CompilationUnit) = {
    newTraverser().traverse(unit.body)
  }   

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

