package com.googlecode.avro
package plugin


import scala.annotation.tailrec
import scala.reflect.internal.Flags._

import ch.usi.inf.l3.piuma.neve.NeveDSL._
import java.util.{Arrays => JArrays}

import scala.collection.JavaConversions._

import org.apache.avro.Schema



@treeTransformer("methodgen") class MethodGen {
  import definitions._

  rightAfter("objectgen")
  plugin ScalaAvroPlugin
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
        else 
          throw new UnsupportedOperationException("Arrays not used for lists: use scala collections instead")
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
      case definitions.IntClass | definitions.LongClass | 
          definitions.BooleanClass | definitions.FloatClass | 
          definitions.DoubleClass => true
      case _ => false
    })

  private def generateGetMethod(impl: ImplDef, clazz: Symbol, instanceVars: List[Symbol]) = {
    val getMethod0 = mkDefDef(OVERRIDE, newTermName("get"), Nil, Nil,
                            ObjectClass.tpe, mkLiteral(null), clazz)
    val getMethod = addParam(mkParam(IntClass.tpe, getMethod0.symbol), getMethod0)
    val arg = mkIdent(getMethod.symbol.paramss.head(0))
    // TODO: throw the avro bad index exception here
    val newTree = mkConstructorCall(Ident(IndexOutOfBoundsExceptionClass), 
                List(q"${arg}.toString"))
      
    val default = List(CaseDef(Ident(nme.WILDCARD), Throw(newTree)))
    val cases = for ((sym, i) <- instanceVars.zipWithIndex) yield {
      CaseDef(Literal(Constant(i)), {
        if (canInline(sym.tpe)) // trivial ones where no conversions are necessary
          castToObject(clazz, sym)
        else {
          val avroTpe = toAvroType(sym.tpe)
          val schema: Tree = 
            if (needsSchemaToConvert(sym.tpe))
              mkApply(
                mkApply(
                  q"getSchema.getFields.get",
                  List(mkSelect(mkLiteral(i), newTermName("schema")))),
                Nil)
            else 
              mkNullLiteral
            val ths = This(clazz)
            val clazzThis: Tree = Select(ths, sym)
            val app = mkApply(
                          Select(ths, newTermName("convert")),
                          List(
                            TypeTree(sym.tpe),
                            TypeTree(avroTpe)),
                      List(schema, clazzThis)) 
            q"${app}.asInstanceOf[Object]"
          }
      })
    }

    val m = mkMatch(arg, cases ++ default)
    getMethod.updateRHS(m)
  }

  private def generateSetMethod(impl: ImplDef, clazz: Symbol, instanceVars: List[Symbol]) = {
    val putMethod0 = mkDefDef(OVERRIDE, newTermName("put"), Nil, Nil,
                            UnitClass.tpe, mkUnitLiteral, clazz)
    val params = List(mkParam(IntClass.tpe, putMethod0.symbol),
                      mkParam(AnyClass.tpe, putMethod0.symbol))
    val putMethod = params.foldLeft(putMethod0)((z, y) => addParam(y, z))
    val arg = mkIdent(putMethod.symbol.paramss.head(0))
    // TODO: throw the avro bad index exception here
    val newTree = mkConstructorCall(Ident(IndexOutOfBoundsExceptionClass), 
                List(q"${arg}.toString"))
      
    val default = List(CaseDef(Ident(nme.WILDCARD), Throw(newTree)))

    val byteBufferTpe = byteBufferClass.tpe
    val utf8Tpe = utf8Class.tpe

    val cases = for ((sym, i) <- instanceVars.zipWithIndex) yield {
      val rhs = if (canInline(sym.tpe)){
          val id = Ident((putMethod.symbol.paramss.head(1)))
          q"${id}.asInstanceOf[${Ident(sym)}]"
        } else {
          val avroTpe = toAvroType(sym.tpe)
          val schema = if (needsSchemaToConvert(sym.tpe))
              mkApply(
                  q"getSchema.getFields.get",
                  List(Select(mkLiteral(i), newTermName("schema"))),
              Nil)
            else 
              mkNullLiteral
          val id = Ident(putMethod.symbol.paramss.head(1))
          mkApply(
              Select(This(clazz), newTermName("convert")),
              List(
                TypeTree(avroTpe),
                  TypeTree(sym.tpe)),
              List(schema, q"${id}.asInstanceOf[avroTpe]"))
        }

      val ths = This(clazz)
      val clazzThis: Tree = Select(ths, sym)
      CaseDef(Literal(Constant(i)),  Assign(clazzThis, rhs))
    }

    val mtch = mkMatch(mkIdent(putMethod.symbol.paramss.head(0)), cases ++ default)
    putMethod.updateRHS(mtch)
  }

  private def generateGetSchemaMethod(clazzTree: ClassDef): Tree = {
    val mthd = mkDefDef(OVERRIDE, newTermName("getSchema"), Nil, Nil,
                            schemaClass.tpe, mkNullLiteral, clazzTree.symbol)
    val newSym = mthd.symbol
    val clazz = clazzTree.symbol


    val innerTree: Tree =  /** Not sure why compiler needs type information (Tree) here */
      if (clazz.owner.isClass) { /** Only when the owner of this class is another class can we perform
        *  the optimization of referencing the companion object */
        debug("--- clazz ---: " + clazz)
        debug("clazz.enclosingTopLevelClass: " + clazz.enclosingTopLevelClass)
        debug("clazz.outerClass: " + clazz.outerClass)
        debug("clazz.enclClass: " + clazz.enclClass)
        if (clazz.enclosingTopLevelClass == clazz) {
          Select(This(companionModuleOf(clazzTree.symbol).moduleClass), 
                  newTermName("schema"))
        } else {
          Select(Select(This(clazz.outerClass), 
            newTermName(clazz.name.toString)), newTermName("schema"))
        }
      } else { /** Fall back to the naive version in the other cases */
        // TODO: change getSchema to be a lazy val here instead (so we can
        // at least cache the invocations)
        warning("Unable to optimize getSchema method for class %s".format(clazz.fullName.toString))
        mkApply(q"org.apache.avro.Schema.parse", List(mkLiteral(retrieveRecordSchema(clazz).get.toString)))
      }
    mthd.updateRHS(innerTree) 
  }

  private def generateGetUnionSchemaMethod(clazzTree: ClassDef, unionSchema: Schema): Tree = {
    val rhs = mkApply(
          q"org.apache.avro.Schema.parse",
          List(mkLiteral(unionSchema.toString)))

    mkDefDef(OVERRIDE, newTermName("getSchema"), Nil, Nil,
                            schemaClass.tpe, rhs, clazzTree.symbol)
}


  def transform(tree: Tree) : Tree = {
    val newTree = tree match {
      case cd @ ClassDef(mods, name, tparams, impl) if (cd.symbol.tpe.parents.contains(avroUnionTrait.tpe)) =>
      //println("FOUND UNION: " + cd.symbol)
      val schema = getOrCreateUnionSchema(cd.symbol, 
        Schema.createUnion(JArrays.asList(retrieveUnionRecords(cd.symbol).map(s => retrieveRecordSchema(s).get).toArray:_*)))
      //println("SCHEMA: " + schema)
      cd.symbol.resetFlag(INTERFACE) /** force compiler to generate backing $class class */
      cd.addMember(generateGetUnionSchemaMethod(cd, schema))
    case cd @ ClassDef(mods, name, tparams, impl) if (cd.symbol.tpe.parents.contains(avroRecordTrait.tpe)) =>
      debug(retrieveRecordSchema(cd.symbol))
      debug(cd.symbol.fullName + "'s enclClass: " + cd.symbol.enclClass)
      debug("owner.enclClass: " + cd.symbol.owner.enclClass)
      debug("enclosingTopLevelClass: " + cd.symbol.enclosingTopLevelClass)
      debug("owner.enclosingTopLevelClass: " + cd.symbol.owner.enclosingTopLevelClass)

      val instanceVars = 
        for (member <- impl.body if isVal(member) || isVar(member)) yield { member.symbol }
      val newMethods = List(
        generateGetMethod(cd, cd.symbol, instanceVars),
        generateSetMethod(cd, cd.symbol, instanceVars),
        generateGetSchemaMethod(cd))

        cd.updateBody(impl.body ++ newMethods)
      case _ => tree
    }
    super.transform(newTree)
  }    
}
