/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.mina.store

import ch.usi.inf.l3._
import mina._
import mina.eval._

private[mina] trait HPEClassWrapper {
  self: HPE with HPEEnvironmentWrapper =>
  import HPEClassWrapper.this.global._

  class SpecializedClasses {
    var set = Set.empty[ImplDef]
    def add(v: ImplDef): Unit = {
      set.filter(_.symbol.name.toString.toLowerCase ==
        v.symbol.name.toString.toLowerCase).isEmpty match {
        case true => set = set + (v)
        case false =>
          set = set.filter(_.symbol.name.toString.toLowerCase !=
            v.symbol.name.toString.toLowerCase) + (v)
      }
    }

    def +(v: ImplDef): SpecializedClasses = {
      add(v)
      this
    }
    def toList(): List[ImplDef] = set.toList
  }

  object SpecializedClasses {
    def apply(vs: ImplDef*): SpecializedClasses = {
      val sp = new SpecializedClasses
      for (v <- vs) {
        sp.add(v)
      }
      sp
    }
  }
  /*
   * ImplDef if a super class of both ClassDef (for classes) and ModuleDef 
   * for (objects)
   */
  class ClassRepr(val tpe: Type, private var classTree: ImplDef = null) {
    private var specialized: Map[Name, DefDef] = Map.empty
    def getNextMethod(base: Name, ctargs: List[Name], ctvals: List[Value]) = {
      val values = nullify(ctvals)
      var tail = values.toString.replaceAll("[\\[\\]]", "")
      tail = values.toString.replaceAll("[\\(\\)]", "")
      var avails = ctargs.toString.replaceAll("[\\[\\]]", "")
      avails = ctargs.toString.replaceAll("[\\(\\)]", "")
      val name = base.toString + "_m$_i$_" + tail + "$_$" + avails + "_n$_a$"
      newTermName(name)
    }

    var isSpecialized = false
    def hasMember(name: Name, t: Type): Boolean = {
      var flag = false
      if (hasClassTree) {
        for (
          m <- classTree.impl.body if (name == m.symbol.name && t =:= m.symbol.tpe)
        ) {
          flag = true
        }
      }
      flag
    }

    def printMembers() = println(classTree.impl.body)

    def getMemberTree(name: Name, t: Type): Tree = {
      var flag = true
      var result: Tree = null
      if (hasClassTree) {
        for (
          m <- classTree.impl.body if (name == m.symbol.name && t =:= m.symbol.tpe)
        ) {
          flag = false
          result = m
        }
      }
      if (flag) {
        throw new HPEError(s"No member in class ${tpe} " +
          s"has a member ${name} with the type ${t}")
      } else
        result
    }

    override def equals(that: Any): Boolean = {
      that match {
        case null => false
        case x: ClassRepr => tpe =:= x.tpe
        case _ => false
      }
    }

    override def hashCode = 71 * 5 + tpe.##

    def tree_=(clazz: ImplDef): Unit = classTree = clazz

    def hasClassTree() = {
      classTree match {
        case null => false
        case _ => true
      }
    }

    def tree: ImplDef = classTree match {
      case null => throw new HPEError(s"""${classTree} is null + ${tpe}""")
      case _ => classTree
    }

    private def nullify(args: List[Value]): List[Value] = {
      var temp: List[Value] = Nil
      for (arg <- args) {
        arg match {
          case x: CTValue => temp = x :: temp
          case _ => temp = Bottom :: temp
        }
      }
      temp.reverse
    }

    def addSpecialized(name: Name, ctargs: List[Name], args: List[Value], method: DefDef) = {
      specialized = specialized + (getNextMethod(name, ctargs, args) -> method)
    }

    def getSpecialized(name: Name, ctargs: List[Name], args: List[Value]): DefDef = {
      specialized(getNextMethod(name, ctargs, args))
    }

    def getSpecializedOption(name: Name, ctargs: List[Name], args: List[Value]): Option[DefDef] = {
      specialized.get(getNextMethod(name, ctargs, args))
    }

    def hasSpecialized(name: Name, ctargs: List[Name], args: List[Value]): Boolean = {
      getSpecializedOption(name, ctargs, args) match {
        case Some(x) => true
        case None => false
      }
    }

    override def toString(): String = tpe.toString
  }

  class ClassBank {

    private var speciazlized: Map[Name, ClassRepr] = Map.empty
    private var allMorphs: Map[Type, SpecializedClasses] = Map.empty

    private def nullify(args: List[Value]): List[Value] = {
      var temp: List[Value] = Nil
      for (arg <- args) {
        arg match {
          case x: CTValue => temp = x :: temp
          case _ => temp = Bottom :: temp
        }
      }
      temp.reverse
    }

    def getAllMorphs(tpe: Type): List[ImplDef] = {
      digraph.getClassRepr(tpe) match {
        case None => Nil
        case Some(clazz) =>
          if (clazz.isSpecialized) Nil
          else
            allMorphs.get(tpe) match {
              case Some(classes) => classes.toList
              case _ => Nil
            }
      }
    }

    def getNextClassName(base: Name, ctargs: List[Name],
      ctvals: List[Value]): TypeName = {
      val vals = nullify(ctvals)
      var tail = vals.toString.replaceAll("[\\[\\]]", "")
      tail = vals.toString.replaceAll("[\\(\\)]", "")
      var avails = ctargs.toString.replaceAll("[\\[\\]]", "")
      avails = ctargs.toString.replaceAll("[\\(\\)]", "")
      val newName = base + "_m$_i$_" + tail + "$_$" + avails + "_n$_a$"
      newTypeName(newName)
    }

    def add(base: Name, tpe: Type, ctargs: List[Name], ctvals: List[Value], clazz: ClassRepr) = {

      speciazlized = speciazlized + (getNextClassName(base, ctargs, ctvals) -> clazz)
      allMorphs = allMorphs.get(tpe) match {
        case None => allMorphs + (tpe -> SpecializedClasses(clazz.tree))
        case Some(l) => allMorphs + (tpe -> (l + clazz.tree))
      }
    }

    def get(base: Name, ctargs: List[Name], ctvals: List[Value]): ClassRepr = {
      speciazlized(getNextClassName(base, ctargs, ctvals))
    }

    def getOption(base: Name, ctargs: List[Name], ctvals: List[Value]): Option[ClassRepr] = {
      speciazlized.get(getNextClassName(base, ctargs, ctvals))
    }

    def has(base: Name, ctargs: List[Name], ctvals: List[Value]): Boolean = {
      getOption(base, ctargs, ctvals) match {
        case Some(x) => true
        case None => false
      }
    }
  }

  class ClassDigraph {
    type C = ClassRepr

    private var companionMap: Map[Type, ClassRepr] = Map.empty
    private var index = 0
    private var nodes = Map.empty[C, Int]
    private var reversed = Map.empty[Int, C]
    private var edges = Map.empty[Int, List[Int]]

    def getCompanionRepr(tpe: Type): List[ClassRepr] = {
      companionMap.get(tpe) match {
        case Some(c) => List(c)
        case _ => Nil
      }
    }
    def getCompanion(tpe: Type): List[ImplDef] = {
      companionMap.get(tpe) match {
        case Some(c) => List(c.tree)
        case _ => Nil
      }
    }
    def addCompanion(tpe: Type, module: ClassRepr): Unit = {
      companionMap = companionMap + (tpe -> module)
    }
    def findCompanionModule(clazz: Symbol): Option[C] = {
      if (clazz.isModule) {
        getClassRepr(clazz.tpe)
      } else {
        val mod = clazz.companionModule
        if (mod != NoSymbol) {
          getClassRepr(mod.tpe)
        } else {
          getCompanionRepr(clazz.tpe) match {
            case x :: Nil => Some(x)
            case _ => None
          }
        }
      }
    }

    def addClass(clazz: C) = {
      nodes.contains(clazz) match {
        case false =>
          nodes = nodes + (clazz -> index)
          reversed = reversed + (index -> clazz)
          edges = edges + (index -> Nil)
          index += 1
        case true if (!clazz.hasClassTree) =>
          val index1 = nodes(clazz)
          val current = reversed(index1)
          if (!current.hasClassTree) {
            nodes = nodes + (clazz -> index1)
            reversed = reversed + (index1 -> clazz)
          }
        case _ =>
      }
    }

    def addSubclass(clazz: C, subclass: C) = {
      val f = getIndex(clazz)
      val t = getIndex(subclass)
      edges = edges + (f -> (t :: edges(getIndex(clazz))))
    }

    def getSubclasses(clazz: C): List[C] = {
      nodes.get(clazz) match {
        case Some(index) =>
          edges(index) match {
            case Nil => Nil
            case cs => cs.map(reversed(_))
          }
        case None => Nil
      }
    }

    def getClassRepr(tpe: Type): Option[ClassRepr] = {
      if (tpe == null) None
      else {
        val temp = new ClassRepr(tpe)
        nodes.get(temp) match {
          case None => None
          case Some(index) =>
            val clazz = reversed(index)
            clazz.hasClassTree match {
              case true => Some(clazz)
              case _ => None
            }

        }
      }
    }

    private def getIndex(node: C): Int = {
      nodes.contains(node) match {
        case false =>
          addClass(node)
          nodes(node)
        case true =>
          nodes(node)
      }
    }
  }

}
