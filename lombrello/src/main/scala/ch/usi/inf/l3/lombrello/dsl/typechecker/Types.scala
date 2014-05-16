package ch.usi.inf.l3.lombrello.dsl.typechecker

/**
 * @author Amanj Sherwany
 * @date 13 May 2014
 */

import ch.usi.inf.l3.lombrello.dsl
import dsl.source.Position
import dsl.Names
import scala.reflect.runtime.{universe => ru}
import scala.reflect.internal.MissingRequirementError


trait Types { self: dsl.Compiler => 

  def neveToScala(tpe: Type): ru.Type = {
    null
  }

  sealed trait Type {
    val name: String


    def =:=(tpe: Type): Boolean
    def <:<(tpe: Type): Boolean
  }

  case class NeveType(name: String, members: List[self.Symbol]) extends Type {
    def declares(sym: Symbol, tpe: Type): Boolean = {
      members.exists((x) => name == tpe.name && (tpe =:= x.tpe || tpe <:< x.tpe))
    }

    // TODO: Implement
    def =:=(tpe: Type): Boolean = true
    def <:<(tpe: Type): Boolean = true
  }

  case class ScalaType(name: String, isClass: Boolean) extends Type {
    val scalaSymbol = isClass match {
      case true =>
        ru.runtimeMirror(getClass.getClassLoader).staticClass(name)
      case _ =>
        try {
          ru.runtimeMirror(getClass.getClassLoader).staticModule(name)
        } catch {
          case ex: MissingRequirementError =>
            ru.runtimeMirror(getClass.getClassLoader).staticPackage(name)
        }
    }

    private val scalaType = scalaSymbol.typeSignature

    private val ms = scalaType.members 

    private val members = ms.map((x: ru.Symbol) => (x, x.typeSignature)).toMap

    private val baseClasses = scalaType.baseClasses.tail.map(_.asClass)
    private val parentTypes = baseClasses.map((x) => ScalaType(x.fullName, !x.isModule))


    def declares(sym: ru.Symbol, tpe: ru.Type): Boolean = {
      members.exists((x) => {
        (x._1.fullName == sym.fullName && (tpe =:= x._2 || tpe <:< x._2))
      }) ||
        parentTypes.foldLeft(false)((z, y) => {
          z || y.declares(sym, tpe)
        })
    }


    // TODO: Implement
    def =:=(tpe: Type): Boolean = false
    def <:<(tpe: Type): Boolean = false
    
  }

  case class TermType(name: String, params: List[self.Symbol], 
        ret: self.Symbol) extends Type {
    
    // TODO: Implement
    def =:=(tpe: Type): Boolean = false
    def <:<(tpe: Type): Boolean = false
  }

  case object NoType extends Type {
    val name: String = Names.NO_TYPE
    def =:=(tpe: Type): Boolean = false
    def <:<(tpe: Type): Boolean = false
  }

}

// TODO: You can type check the dsl using scala's reflect api
// val ms = ru.runtimeMirror(myClassLoader).staticClass(
//                                         "model.Model").typeSignature.members
// ms.foreach((x: ru.Symbol) => ())
