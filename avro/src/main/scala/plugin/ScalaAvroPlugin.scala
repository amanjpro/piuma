package com.googlecode.avro
package plugin

import scala.language.implicitConversions

import scala.collection.mutable.{Map,HashMap,HashSet,MutableList,ListBuffer}

import ch.usi.inf.l3.piuma.neve.NeveDSL._
import org.apache.avro.{specific, Schema}
import Schema.Type
import specific.SpecificRecord
import scala.reflect.internal.Flags._


@plugin(Extender,
        CtorRetype,
        UnionDiscover,
        UnionClosure,
        SchemaCreate,
        SchemaGen,
        ObjectGen,
        MethodGen) class ScalaAvroPlugin {

  val beforeFinder = "parser"
  val name = "avro-scala-plugin"
  describe("Support for auto generation of Avro Records")

  val classToSchema: Map[Symbol, Schema] = new HashMap[Symbol, Schema]
  val unionToExtenders: Map[Symbol, List[Symbol]] = new HashMap[Symbol, List[Symbol]]
  val unionToSchemas: Map[Symbol, Schema] = new HashMap[Symbol, Schema]
  val unitMap: Map[CompilationUnit, List[Symbol]] = new HashMap[CompilationUnit, List[Symbol]]
  val companionModuleMap: Map[String, Symbol] = new HashMap[String, Symbol]
  val companionClassMap: Map[String, Symbol] = new HashMap[String, Symbol]

  def debug(a: AnyRef) {
    if (settings.debug.value) 
      println(a)
  }

  def warn(a: AnyRef) {
    println(a)
  }


  // TODO: the following values are lazy so that a NPE doesn't get thrown
  // upon instantiation

  /** Definitions doesn't contain one for MapClass */
  lazy val MapClass = rootMirror.getClassByName(newTypeName("scala.collection.immutable.Map"))

  /** Avro Scala Plugin Traits */
  lazy val avroRecordTrait = rootMirror.getClassByName(newTypeName("com.googlecode.avro.marker.AvroRecord"))
  lazy val avroUnionTrait = rootMirror.getClassByName(newTypeName("com.googlecode.avro.marker.AvroUnion"))

  /** Avro Extra Primitive Types */
  lazy val byteBufferClass = rootMirror.getClassByName(newTypeName("java.nio.ByteBuffer"))
  lazy val utf8Class = rootMirror.getClassByName(newTypeName("org.apache.avro.util.Utf8"))

  /** Avro Internal Types */
  lazy val GenericArrayClass = rootMirror.getClassByName(newTypeName("org.apache.avro.generic.GenericArray"))
  lazy val schemaClass = rootMirror.getClassByName(newTypeName("org.apache.avro.Schema"))
  lazy val SpecificRecordIface = rootMirror.getClassByName(newTypeName("org.apache.avro.specific.SpecificRecord"))
  lazy val SpecificRecordBaseClass = rootMirror.getClassByName(newTypeName("org.apache.avro.specific.SpecificRecordBase"))
  lazy val JMapClass = rootMirror.getClassByName(newTypeName("java.util.Map"))

  /** Scala Avro Internal types */
  lazy val ScalaSpecificRecord = rootMirror.getClassByName(newTypeName("com.googlecode.avro.runtime.ScalaSpecificRecord"))
  lazy val AvroConversions = rootMirror.getClassByName(newTypeName("com.googlecode.avro.runtime.HasAvroConversions"))
  lazy val GenericArrayWrapperClass = rootMirror.getClassByName(newTypeName("com.googlecode.avro.runtime.GenericArrayWrapper"))
  def retrieveRecordSchema(name: Symbol): Option[Schema] = classToSchema.get(name)

  def addRecordSchema(name: Symbol, schema: Schema) {
    assert(schema.getType == Type.RECORD)
    classToSchema += name -> schema
  }

  def isRecord(sym: Symbol) = classToSchema.contains(sym)

  def isExternalRecord(sym: Symbol) = sym.tpe <:< SpecificRecordIface.tpe

  def retrieveExternalRecordSchema(sym: Symbol): Schema = {
    val clazz = Class.forName(sym.fullName.toString).asInstanceOf[Class[SpecificRecord]]
    clazz.newInstance.getSchema
  }

  def retrieveUnionRecords(name: Symbol): List[Symbol] = unionToExtenders.get(name) match {
    case Some(l) => l
    case None => Nil
  }

  def addUnionRecord(name: Symbol, schema: Symbol) {
    unionToExtenders.get(name) match { 
      case Some(buf) =>
        unionToExtenders += name -> (buf ::: List(schema))
      case None =>
        unionToExtenders += name -> List(schema)
    }
  }

  def isUnion(sym: Symbol) = unionToExtenders.contains(sym)

  def getOrCreateUnionSchema(sym: Symbol, schema: => Schema): Schema =
    unionToSchemas.getOrElseUpdate(sym, schema)

  def getUnionSchema(sym: Symbol) = 
    unionToSchemas.get(sym)

  def retrieveUnions(unit: CompilationUnit): List[Symbol] = unitMap.get(unit) match {
    case Some(l) => l
    case None => Nil
  }

  def addUnionToUnit(unit: CompilationUnit, union: Symbol) {
    unitMap.get(unit) match {
      case Some(buf) =>
        unitMap += unit -> (buf ::: List(union))
      case None =>
        unitMap += unit -> List(union)
    }
  }

  def companionClassOf(module: Symbol): Symbol = {
    val companionClass0 = module.companionClass
    if (companionClass0 != NoSymbol) {
      companionClass0
    } else {
      companionClassMap.get(module.fullName).getOrElse(NoSymbol)
    }
  }

  def companionModuleOf(clazz: Symbol): Symbol = {
    val companionModule0 = clazz.companionModule
    if (companionModule0 != NoSymbol) {
      companionModule0
    } else {
      companionModuleMap.get(clazz.fullName).getOrElse(NoSymbol)
    }
  }
}
