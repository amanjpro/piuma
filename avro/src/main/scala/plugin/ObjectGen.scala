package com.googlecode.avro
package plugin
//
// import scala.tools._
// import nsc.Global
// import nsc.Phase
// import nsc.plugins.Plugin
// import nsc.plugins.PluginComponent
// import nsc.transform.Transform
// import nsc.transform.InfoTransform
// import nsc.transform.TypingTransformers
// import nsc.symtab.Flags._
// import nsc.util.Position
// import nsc.util.NoPosition
// import nsc.ast.TreeDSL
// import nsc.typechecker
import scala.annotation.tailrec
import scala.language.postfixOps

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.collection.JavaConversions._
import scala.reflect.internal.Flags._

@phase("objectgen") class ObjectGen {
  // import global.class _
  import definitions._
  // val global : ScalaAvroPlugin.this.global.type = ScalaAvroPlugin.this.global


  // val classToSchema = ScalaAvroPlugin.this.classToSchema
  // val unionToExtenders = ScalaAvroPlugin.this.unionToExtenders
  // val unionToSchemas = ScalaAvroPlugin.this.unionToSchemas
  // val unitMap = ScalaAvroPlugin.this.unitMap
  // val companionModuleMap = ScalaAvroPlugin.this.companionModuleMap
  // val companionClassMap = ScalaAvroPlugin.this.companionClassMap


  // val runsAfter = List[String]("schemagen")
  rightAfter("schemagen")
  plugin ScalaAvroPlugin
  // val phaseName = "objectgen"
  // def newTransformer(unit: CompilationUnit) = new ObjectGenTransformer(unit)    

  // class ObjectGenTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
  // import CODE._

  private def pimpModuleDef(md: ModuleDef): Tree = {
    debug("pimping the module def: " + md)

    val impl  = md.impl
    val clazz = md.symbol.moduleClass

    val valSym = clazz.newValue(newTermName("schema "), clazz.pos.focus)
    valSym setFlag (PRIVATE | LOCAL)
    valSym setInfo schemaClass.tpe
    clazz.info.decls enter valSym

    val valDef = localTyper.typed {
      ValDef(valSym,
        Apply(q"org.apache.avro.Schema.parse",
          // Ident(newTermName("org")) DOT 
          //   newTermName("apache")   DOT
          //   newTermName("avro")     DOT
          //   newTermName("Schema")   DOT
          //   newTermName("parse"),
          List(Literal(Constant(retrieveRecordSchema(companionClassOf(md.symbol)).get.toString)))))
    }

    // TODO: See if this is still necessary - Amanj
    // !!! HACK !!!
    // val owner0 = localTyper.context1.enclClass.owner

    // localTyper.context1.enclClass.owner = clazz

    val getterSym = clazz.newMethod(newTermName("schema"), clazz.pos.focus)
    getterSym setFlag (METHOD | STABLE | ACCESSOR)
    getterSym setInfo MethodType(getterSym.newSyntheticValueParams(Nil), schemaClass.tpe)
    clazz.info.decls enter getterSym

    val getDef = DefDef(getterSym, Block(Nil, Select(This(clazz), newTermName("schema "))))

    // !!! RESTORE HACK !!!

    // localTyper.context1.enclClass.owner = owner0

    val impl0 = treeCopy.Template(impl, impl.parents, impl.self, valDef :: getDef :: impl.body)
    localTyper.typed { treeCopy.ModuleDef(md, md.mods, md.name, impl0)}
    }

  def transform(tree: Tree) : Tree = {
    val newTree = tree match {
      case md @ ModuleDef(mods, name, impl) if (companionClassOf(md.symbol).tpe.parents.contains(avroRecordTrait.tpe)) =>

        debug("Adding module to companionModule map")
        companionModuleMap += md.symbol.fullName -> md.symbol
        debug("companionModuleMap: " + companionModuleMap)

        val hasSchemaDef = ! ( impl.body.filter { 
          case ValDef(_, n, _, _) if (n.toString == "schema ") => true
          case _ => false
        } isEmpty )

        if (hasSchemaDef)
          throw new AssertionError("Object cannot have schema member already defined: %s".format(md.symbol))

        pimpModuleDef(md)
          case _ => tree
        }
    super.transform(newTree)
  }    
  // }

  }
