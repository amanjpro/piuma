package com.googlecode.avro
package plugin


import scala.annotation.tailrec
import scala.reflect.internal.Flags._

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import java.util.{Arrays => JArrays}

import scala.collection.JavaConversions._

import org.apache.avro.Schema



@phase("methodgen") class MethodGen {
  // import global._
  import definitions._
  // import CODE._


  // val classToSchema = ScalaAvroPlugin.this.classToSchema
  // val unionToExtenders = ScalaAvroPlugin.this.unionToExtenders
  // val unionToSchemas = ScalaAvroPlugin.this.unionToSchemas
  // val unitMap = ScalaAvroPlugin.this.unitMap
  // val companionModuleMap = ScalaAvroPlugin.this.companionModuleMap
  // val companionClassMap = ScalaAvroPlugin.this.companionClassMap

  // val runsAfter = List[String]("objectgen")
  rightAfter("objectgen")
  plugin ScalaAvroPlugin
  // val phaseName = "methodgen"
  // def newTransformer(unit: CompilationUnit) = new MethodGenTransformer(unit)    

  // class MethodGenTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

  /** this.sym.asInstanceOf[java.lang.Object] */
   private def castToObject(clazz: Symbol, sym: Symbol): Tree = {
     val c = Select(This(clazz), sym)
     q"${c}.asInstanceOf[Object]"
     }

   private def construct(tpe: Type, args: List[Type]) = tpe match {
     case TypeRef(pre, sym, _) => TypeRef(pre, sym, args)
   }

   private def toAvroType(tpe: Type): Type = {
     if (tpe.typeSymbol.isSubClass(definitions.TraversableClass)) {
       // TODO: this is a bit fragile (for instance, what if somebody defines
       // their map as a type alias. there won't be any type params
       val size = tpe.typeArgs.size
       size match {
         case 1 =>
           construct(GenericArrayClass.tpe, List(toAvroType(tpe.typeArgs.head)))
         case 2 =>
           construct(JMapClass.tpe, List(utf8Class.tpe, toAvroType(tpe.typeArgs.tail.head)))
         case _ =>
           throw new UnsupportedOperationException("Unsupported collection type: " + tpe)
   }
   } else tpe.typeSymbol match {
     case definitions.OptionClass =>
       toAvroType(tpe.typeArgs.head)
     case definitions.StringClass => utf8Class.tpe 
     case definitions.ArrayClass  =>
       if (tpe.typeArgs.head.typeSymbol == definitions.ByteClass)
         byteBufferClass.tpe
       else throw new UnsupportedOperationException("Arrays not used for lists: use scala collections instead")
     case definitions.ByteClass | definitions.BoxedByteClass | 
   definitions.ShortClass | definitions.BoxedShortClass | 
   definitions.CharClass | definitions.BoxedCharacterClass | 
   definitions.IntClass | definitions.BoxedIntClass  => BoxedIntClass.tpe 
     case definitions.BooleanClass | definitions.BoxedBooleanClass => BoxedBooleanClass.tpe
     case definitions.LongClass | definitions.BoxedLongClass  => BoxedLongClass.tpe
     case definitions.FloatClass | definitions.BoxedFloatClass  => BoxedFloatClass.tpe
     case definitions.DoubleClass | definitions.BoxedDoubleClass => BoxedDoubleClass.tpe
     case _ => tpe
     }
   }

   private def needsSchemaToConvert(tpe: Type): Boolean = 
     tpe.typeSymbol.isSubClass(definitions.TraversableClass) || 
   (tpe.typeSymbol == definitions.OptionClass && needsSchemaToConvert(tpe.typeArgs.head))

   private def canInline(tpe: Type) = 
     tpe == toAvroType(tpe) || (tpe.typeSymbol match {
       case definitions.IntClass | definitions.LongClass | definitions.BooleanClass | definitions.FloatClass | definitions.DoubleClass => true
       case _ => false
     })

   private def generateGetMethod(templ: Template, clazz: Symbol, instanceVars: List[Symbol]) = {
     val newSym = clazz.newMethod(newTermName("get"), clazz.pos.focus)
     newSym setFlag SYNTHETIC | OVERRIDE 
     newSym setInfo MethodType(newSym.newSyntheticValueParams(List(/*Boxed*/ IntClass.tpe)), /*Any*/ObjectClass.tpe)
     clazz.info.decls enter newSym 

     val arg = localTyper.typed {Ident(newSym.paramss.head(0))}//List(0)//newSym ARG 0
     // TODO: throw the avro bad index exception here
     val newTree = Select(New(Ident(IndexOutOfBoundsExceptionClass)), nme.CONSTRUCTOR)
     val default = List(CaseDef(Ident(nme.WILDCARD), Throw(Apply(newTree, List(arg)))))
     val cases = for ((sym, i) <- instanceVars.zipWithIndex) yield {
       CaseDef(Literal(Constant(i)), {
         if (canInline(sym.tpe)) // trivial ones where no conversions are necessary
           castToObject(clazz, sym)
         else {
           val avroTpe = toAvroType(sym.tpe)
           val schema: Tree = 
             if (needsSchemaToConvert(sym.tpe))
               Apply(
                 Apply(
                   q"getSchema.getFields.get",
                   // This(clazz) DOT newTermName("getSchema") DOT newTermName("getFields") DOT newTermName("get"),
                   List(Select(Literal(Constant(i)), newTermName("schema")))),
                 Nil)
               else Literal(Constant(null))
               val ths = This(clazz)
               val clazzThis: Tree = Select(ths, sym)
               clazzThis.substituteThis(clazz, ths)
               val app = Apply(
                 TypeApply(
                   Select(This(clazz), newTermName("convert")),
                   List(
                     TypeTree(sym.tpe),
                     TypeTree(avroTpe))),
                 List(schema, clazzThis)) 
               q"${app}.asInstanceOf[Object]"
               }
       })
       }

     val m = localTyper.typed {Match(arg, cases ++ default)}
     DefDef(newSym, m)// arg MATCH { cases ::: default : _* })
     }

   private def generateSetMethod(templ: Template, clazz: Symbol, instanceVars: List[Symbol]) = {
     val newSym = clazz.newMethod(newTermName("put"), clazz.pos.focus)
     newSym setFlag SYNTHETIC | OVERRIDE
     newSym setInfo MethodType(newSym.newSyntheticValueParams(List(IntClass.tpe, AnyClass.tpe)), UnitClass.tpe)
     clazz.info.decls enter newSym 

     // TODO: throw avro bad index class
     val newTree = Select(New(Ident(IndexOutOfBoundsExceptionClass)), nme.CONSTRUCTOR)
     val default = List(CaseDef(Ident(nme.WILDCARD),  
                       Ident(newSym.paramss.head(0))))

     val byteBufferTpe = byteBufferClass.tpe
     val utf8Tpe = utf8Class.tpe

     val cases = for ((sym, i) <- instanceVars.zipWithIndex) yield {
       val rhs =
         if (canInline(sym.tpe)){
           val id = Ident((newSym.paramss.head(1)))
           q"${id}.asInstanceOf[${Ident(sym)}]"
         } else {
           val avroTpe = toAvroType(sym.tpe)
           val schema = 
             if (needsSchemaToConvert(sym.tpe))
               Apply(
                 Apply(
                   q"getSchema.getFields.get",
                   // Select(This(clazz), newTermName("getSchema"))newTermName("getFields") DOT newTermName("get"),
                   List(Select(Literal(Constant(i)), newTermName("schema")))),
                 Nil)
               else Literal(Constant(null))
               val id = Ident(newSym.paramss.head(1))
               Apply(
                 TypeApply(
                   Select(This(clazz), newTermName("convert")),
                   List(
                     TypeTree(avroTpe),
                     TypeTree(sym.tpe))),
                 List(schema, q"${id}.asInstanceOf[avroTpe]"))
                 }

                 val ths = This(clazz)
                 val clazzThis: Tree = Select(ths, sym)
                 clazzThis.substituteThis(clazz, ths)
                 CaseDef(Literal(Constant(i)),  Assign(clazzThis, rhs))
         }

     // atOwner(clazz)(localTyper.typed {
     val mtch = localTyper.typed {Match(localTyper.typed{Ident(newSym.paramss.head(0))}, 
     cases ++ default)}
   // mtch.setType(newSym.info)
     DefDef(newSym, mtch)
   // })
     }

   private def generateGetSchemaMethod(clazzTree: ClassDef): Tree = {
     val clazz = clazzTree.symbol
     val newSym = clazz.newMethod(newTermName("getSchema"), clazz.pos.focus)
     newSym setFlag SYNTHETIC | OVERRIDE
     newSym setInfo MethodType(newSym.newSyntheticValueParams(Nil), schemaClass.tpe)
     clazz.info.decls enter newSym 
     val innerTree: Tree =  /** Not sure why compiler needs type information (Tree) here */
    if (clazz.owner.isClass) { /** Only when the owner of this class is another class can we perform
                                *  the optimization of referencing the companion object */
                               debug("--- clazz ---: " + clazz)
                               debug("clazz.enclosingTopLevelClass: " + clazz.enclosingTopLevelClass)
                               debug("clazz.outerClass: " + clazz.outerClass)
                               debug("clazz.enclClass: " + clazz.enclClass)
                               if (clazz.enclosingTopLevelClass == clazz) {
                                 val ths = This(companionModuleOf(clazzTree.symbol).moduleClass)
                                 val slct = Select(ths, newTermName("schema"))
                                 slct.substituteThis(clazz, ths)
                                 slct
                               } else {
                                 val ths = This(clazz.outerClass)
                                 val slct = Select(Select(ths, newTermName(clazz.name.toString)), newTermName("schema"))
                                 slct.substituteThis(clazz, ths)
                                 slct
                               }
                               } else { /** Fall back to the naive version in the other cases */
                              // TODO: change getSchema to be a lazy val here instead (so we can
                              // at least cache the invocations)
                              warning("Unable to optimize getSchema method for class %s".format(clazz.fullName.toString))
                              Apply(q"org.apache.avro.Schema.parse", List(Literal(Constant(retrieveRecordSchema(clazz).get.toString))))
                              // Ident(newTermName("org")) DOT 
                              //   newTermName("apache")   DOT
                              //   newTermName("avro")     DOT
                              //   newTermName("Schema")   DOT
                              //   newTermName("parse"),
                              // List(LIT(retrieveRecordSchema(clazz).get.toString)))
    }
    localTyper.typed {
      DefDef(newSym, innerTree)
                               }
    }

   private def generateGetUnionSchemaMethod(clazzTree: ClassDef, unionSchema: Schema): Tree = {
     val clazz = clazzTree.symbol
     val newSym = clazz.newMethod(newTermName("getSchema"), clazz.pos.focus)
     newSym setFlag SYNTHETIC | OVERRIDE
     newSym setInfo MethodType(newSym.newSyntheticValueParams(Nil), schemaClass.tpe)
     clazz.info.decls enter newSym 

     localTyper.typed {
       DefDef(newSym, 
         Apply(
           q"org.apache.avro.Schema.parse",
           List(Literal(Constant(unionSchema.toString)))))
       // Ident(newTermName("org")) DOT 
       //   newTermName("apache")   DOT
       //   newTermName("avro")     DOT
       //   newTermName("Schema")   DOT
       //   newTermName("parse"),
       // List(LIT(unionSchema.toString))))
   }
     }


   def transform(tree: Tree) : Tree = {
     val newTree = tree match {
       case cd @ ClassDef(mods, name, tparams, impl) if (cd.symbol.tpe.parents.contains(avroUnionTrait.tpe)) =>
         //println("FOUND UNION: " + cd.symbol)
         val schema = getOrCreateUnionSchema(cd.symbol, 
           Schema.createUnion(JArrays.asList(retrieveUnionRecords(cd.symbol).map(s => retrieveRecordSchema(s).get).toArray:_*)))
         //println("SCHEMA: " + schema)
         cd.symbol.resetFlag(INTERFACE) /** force compiler to generate backing $class class */
        val newMethod = List(generateGetUnionSchemaMethod(cd, schema))
        val newImpl = treeCopy.Template(impl, impl.parents, impl.self, newMethod ::: impl.body)
        localTyper.typed {treeCopy.ClassDef(cd, mods, name, tparams, newImpl)}
       case cd @ ClassDef(mods, name, tparams, impl) if (cd.symbol.tpe.parents.contains(avroRecordTrait.tpe)) =>
         debug(retrieveRecordSchema(cd.symbol))
         debug(cd.symbol.fullName + "'s enclClass: " + cd.symbol.enclClass)
         debug("owner.enclClass: " + cd.symbol.owner.enclClass)
         debug("enclosingTopLevelClass: " + cd.symbol.enclosingTopLevelClass)
         debug("owner.enclosingTopLevelClass: " + cd.symbol.owner.enclosingTopLevelClass)

         println(cd)
         val instanceVars = 
           for (member <- impl.body if isValDef(member)) yield { member.symbol }
         val newMethods = List(
           generateGetMethod(impl, cd.symbol, instanceVars),
           generateSetMethod(impl, cd.symbol, instanceVars),
           generateGetSchemaMethod(cd))

         val newImpl = treeCopy.Template(impl, impl.parents, impl.self, newMethods ::: impl.body)
         localTyper.typed{treeCopy.ClassDef(cd, mods, name, tparams, newImpl)}
       case _ => tree
         }
     super.transform(newTree)
   }    
   // }
}
