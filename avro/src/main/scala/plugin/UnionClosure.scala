package com.googlecode.avro
package plugin

// import scala.tools.nsc._

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.reflect.internal.Flags._
import org.apache.avro.Schema



@checker("unionclosure") class UnionClosure {
  // val classToSchema = ScalaAvroPlugin.this.classToSchema
  // val unionToExtenders = ScalaAvroPlugin.this.unionToExtenders
  // val unionToSchemas = ScalaAvroPlugin.this.unionToSchemas
  // val unitMap = ScalaAvroPlugin.this.unitMap
  // val companionModuleMap = ScalaAvroPlugin.this.companionModuleMap
  // val companionClassMap = ScalaAvroPlugin.this.companionClassMap

  // val runsAfter = List[String]("uniondiscover")
  rightAfter("uniondiscover")
  plugin ScalaAvroPlugin
  // val phaseName = "unionclosure"

  // def newPhase(prev: Phase): Phase = new TraverserPhase(prev)

  // class TraverserPhase(prev: Phase) extends StdPhase(prev) {
  private var unit: CompilationUnit = _
  private def check(_unit: CompilationUnit) = {
    unit = _unit
    debug("Union for comp unit: " + unit)
    debug(retrieveUnions(unit))
    newTraverser().traverse(unit.body)
  }   

  private def newTraverser(): Traverser = new ForeachTreeTraverser(check1)

  private def check1(tree: Tree): Unit = tree match {
    case cd @ ClassDef(_, _, _, _) if (cd.symbol.tpe.parents.contains(avroRecordTrait.tpe)) =>
      debug("Running union closure on class: " + cd.symbol.fullName)
      val unitUnions = retrieveUnions(unit)
      unitUnions.
        filter(unionSym => {
          debug("Comparing cd.symbol.tpe " + cd.symbol.tpe + " to " + unionSym.tpe)
          cd.symbol.tpe <:< unionSym.tpe }).
        foreach(unionSym => addUnionRecord(unionSym, cd.symbol))
    case cd @ ClassDef(mods,_,_,_) =>
      debug("Skipped class: " + cd.symbol.fullName)
    case _ => ()
  }
  // }

}
