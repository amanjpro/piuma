package com.googlecode.avro
package plugin

import scala.language.postfixOps
import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.reflect.internal.Flags._


@info("extender") class Extender {

  after(List("typer"))
  before(List("superaccessors"))
  plugin ScalaAvroPlugin


  def transformInfo(sym: Symbol, tpe: Type): Type = tpe match {
    case ClassInfoType(parents, decls, clazz) if (!clazz.isPackageClass && clazz.tpe.parents.contains(avroRecordTrait.tpe)) =>
      // 1) warn if current parent is not java.lang.Object AND if it is not a
      // subtype of SpecificRecordBase
      val (car, cdr) = clazz.tpe.parents.splitAt(1)
      if (car.head != definitions.ObjectClass.tpe && !(car.head <:< SpecificRecordBaseClass.tpe))
        warn("Replacing inheritance of non specific record base type")
      ClassInfoType(List(SpecificRecordBaseClass.tpe, AvroConversions.tpe) ::: cdr, decls, clazz)
    case _ => tpe
  }

  // class ExtenderTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
  // import CODE._

  private val DefaultValues = Map(
    definitions.IntClass     -> mkLiteral(0),
    definitions.LongClass    -> mkLiteral(0L),
    definitions.FloatClass   -> mkLiteral(0.0F),
    definitions.DoubleClass  -> mkLiteral(0.0),
    definitions.BooleanClass -> mkLiteral(false),
    definitions.ShortClass   -> mkLiteral(0),
    definitions.ByteClass    -> mkLiteral(0),
    definitions.CharClass    -> mkLiteral(0))

  private def isCtor(tree: Tree): Boolean = {
    (tree.symbol ne null) && tree.symbol.name == nme.CONSTRUCTOR
  }

  private def mapToDefaults(params: List[Symbol]) =
    params.map(v => DefaultValues.get(v.tpe.typeSymbol.asClass).getOrElse(Literal(Constant(null))))


  private def preTransform(tree: Tree): Tree = tree match {
    case cd @ ClassDef(mods, name, tparams, impl) 
              if (cd.symbol.tpe.parents.contains(avroRecordTrait.tpe)) =>

      // check that this annotation is a case class
      if (!cd.symbol.hasFlag(CASE))
        throw new NonCaseClassException(name.toString)

      // todo: for case objects, throw exception

      debug("Extending class: " + name.toString)

      val ctors = for (member <- impl.body if isCtor(member)) yield { member.symbol }

      assert (!ctors.isEmpty)

      val containsDefaultCtor = !ctors.map(_.info).filter {
        case MethodType(Nil, MethodType(_, _)) =>
          /** case class Foo()(...) */
           false
        case MethodType(Nil, _) => 
          /** case class Foo() */
           true
        case _ => false
      }.isEmpty

      val ctor = if (containsDefaultCtor) {
        None
      } else {
        // TODO: not sure if this pos stuff is really how we're supposed
        // to be manipulating

        val constructor = mkConstructor(cd, Nil)

        debug("clazz.caseFieldAccessors: " + cd.symbol.caseFieldAccessors)
        debug("clazz.primaryConstructor.tpe.paramTypes: " + cd.symbol.primaryConstructor.tpe.paramTypes)
        debug("clazz.primaryConstructor.tpe.resultType: " + cd.symbol.primaryConstructor.tpe.resultType)
        debug("clazz.primaryConstructor.tpe.finalResultType: " + cd.symbol.primaryConstructor.tpe.finalResultType)
        debug("clazz.primaryConstructor.tpe.boundSyms: " + cd.symbol.primaryConstructor.tpe.boundSyms)


        // TODO: Can I generalize this? -- Amanj
        val innerCtorTpe = cd.symbol.primaryConstructor.tpe
        val innerParamDefaults = mapToDefaults(innerCtorTpe.params)


        val apply0 = mkApply(Select(This(cd.symbol), nme.CONSTRUCTOR), innerParamDefaults)

        val apply = innerCtorTpe.resultType match {
          case MethodType(outerParams, _) =>
            /** primaryCtor is curried */
            mkApply(apply0, mapToDefaults(outerParams))
          case _ =>
            /** primaryCtor is not curried */
             apply0
        }

            
        Some(constructor.updateRHS(Block(List(apply), mkUnitLiteral)))
      }



      val specificRecordBase = 
        toTypedSelectTree("org.apache.avro.specific.SpecificRecordBase")

      val avroConversions = 
        toTypedSelectTree("com.googlecode.avro.runtime.HasAvroConversions")

      val (car, cdr) = impl.parents.splitAt(1)
      val newParents = List(specificRecordBase.symbol.toType, 
                        avroConversions.symbol.toType) ++ cdr.map(_.symbol.toType)
      ctor match {
        case Some(x) => 
          cd.updateParents(newParents).addMember(x)
        case _ => cd.updateParents(newParents)
      }
    case _ => tree
  }

  private def toTypedSelectTree(s: String): Tree = {
    if ((s eq null) || s.isEmpty)
      throw new IllegalArgumentException("Bad FQDN")
    val (car, cdr) = s.split("\\.").toList.splitAt(1)
    if (cdr isEmpty)
      throw new IllegalArgumentException("Nothing to select: " + s)
    else {
      val sym = rootMirror.getModuleByName(newTermName(car.head))
      val first = (car.head, Ident(newTermName(car.head)) setSymbol sym setType sym.tpe)
      cdr.zipWithIndex.foldLeft[(String,Tree)](first)((tuple1, tuple2) => {
        val (name, tree) = tuple1
        val (sel, idx) = tuple2
        val newName = name + "." + sel
        val sym = if (idx == cdr.length - 1)
          rootMirror.getClassByName(newTypeName(newName))
        else 
          rootMirror.getModuleByName(newTermName(newName))
        (newName, Select(tree, 
          if (idx == cdr.length - 1) newTypeName(sel) 
          else newTermName(sel)) setSymbol sym setType sym.tpe)
      })._2
    }
  }

  def transform(tree: Tree): Tree = {
    val t = preTransform(tree)
    super.transform(t)
  }
}
