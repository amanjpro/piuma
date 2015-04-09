package com.googlecode.avro
package plugin

import scala.annotation.tailrec
import scala.language.postfixOps

import ch.usi.inf.l3.piuma.neve.NeveDSL._
import scala.collection.JavaConversions._
import scala.reflect.internal.Flags._

@treeTransformer("objectgen") class ObjectGen {
  import definitions._

  rightAfter("schemagen")
  plugin ScalaAvroPlugin
  private def pimpModuleDef(md: ModuleDef): Tree = {
    debug("pimping the module def: " + md)

    val rhs = mkApply(q"org.apache.avro.Schema.parse",
          List(mkLiteral(retrieveRecordSchema(companionClassOf(md.symbol)).get.toString)))
    val valDef = mkVal(md.symbol.moduleClass,
                        "schema ",
                        rhs)
    val accessors = mkSetterAndGetter(valDef)
    val members = accessors match {
      case None => Nil
      case Some((g, s)) => List(g, s)
    }
    members.foldLeft(md)((z, y) => {
      z.addMember(y)
    })
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

}
