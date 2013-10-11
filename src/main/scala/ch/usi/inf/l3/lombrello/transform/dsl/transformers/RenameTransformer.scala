package ch.usi.inf.l3.lombrello.transform.dsl.transformers

import ch.usi.inf.l3.lombrello.transform.dsl.TransformerPluginComponent
import scala.reflect.internal.Flags

/**
 * What can be renamed, only the trees that already have a name:
 *
 * 1- Bind(name, body)
 * 2- ClassDef(mods, name, tparams, impl)
 * 3- DefDef(mods, name, tparams, vparamss, tpt, rhs)
 * 4- ModuleDef(mods, name, impl)
 * 5- PackageDef(pid, stats) ????
 * 6- TypeDef(mods, name, tparams, rhs)
 * 7- ValDef(mods, name, tpt, rhs) ==> Done
 *
 *
 * ImportSelector, Ident, LabelDef, Select, SelectFromTypeTree
 */
trait RenameTransformerCake {
  renamer: TransformerPluginComponent =>
  trait RenameTransformer {
    self: renamer.TransformerComponent =>

    import renamer.global._

    final def canRename(tree: Tree, newName: Name): Boolean = {
      // Renaming is not allowed after Pickler phase. see bug #1
      val beforePickler = !isAfterPickler
      println(beforePickler)
      tree match {
        case x: ValDef =>
          val vsym = tree.symbol

          val valid = if (vsym.hasGetter) {
            val gtr = vsym.getter(vsym.owner)
            val getterExists = isNotAlreadyDefined(vsym, newName)
            val setterExists = if (vsym.isVar) {
              val strName = nme.getterToSetter(newName)
              val str = vsym.setter(vsym.owner)
              isNotAlreadyDefined(vsym, strName)
            } else {
              false
            }

            !getterExists && !setterExists
          } else {
            true
          }

          val vname = nme.getterToLocal(newName)
          println(vname)
          val noDuplicate = isNotAlreadyDefined(vsym, vname)

          println(valid + "   " + noDuplicate)
          beforePickler && valid && noDuplicate
        case x: ClassDef =>
          val csym = x.symbol
          val msym = csym.companionModule

          val validModule = msym match {
            case NoSymbol => true
            case _ =>
              val msymClass = msym.moduleClass
              val mName = newName.toTermName
              msymClass.name = mName

              msym.name = mName

              isNotAlreadyDefined(msym, mName)
          }

          val noDuplicate = isNotAlreadyDefined(csym, newName)
          println("HERE WHAT HAPPENS???" + validModule + "   " + noDuplicate)
          validModule && beforePickler && noDuplicate
        case _ => false
      }
    }

    /**
     * Renaming a class also renames its companion object
     */
    final def rename(tree: ClassDef, newName: TypeName): Tree = {

      val csym = tree.symbol
      val msym = csym.companionModule

      if (!canRename(tree, newName)) {
        if (isAfterPickler) {
          picklerRenamingException
        } else {
          duplicateException(newName)
        }
      }

      msym match {
        case NoSymbol => ()
        case _ =>
          val msymClass = msym.moduleClass
          val mName = newName.toTermName
          msymClass.name = mName

          msym.name = mName
      }

      csym.name = newName

      val cdef = tree.copy(name = newName).setSymbol(csym)
      localTyper.atOwner(csym.owner).typed(cdef)
    }

    /**
     * Renaming a field, also renames its getters and setters
     */
    final def rename(tree: ValDef, newName: TermName): Tree = {
      val vsym = tree.symbol.asTerm
      val vname = nme.getterToLocal(newName)
      val setterName = nme.getterToSetter(newName)

      if (!canRename(tree, newName)) {
        if (isAfterPickler) {
          picklerRenamingException
        } else {
          duplicateException(newName)
        }
      }

      // You cannot rename a variable and its accessors unless the new name is
      // unique within its bounds

      if (vsym.hasGetter) {
        val gtr = vsym.getter(vsym.owner)
        gtr.name = newName
        if (vsym.isVar) {
          val str = vsym.setter(vsym.owner)
          str.name = setterName
        }
      }

      vsym.name = vname
      val vdef = tree.copy(name = newName).setSymbol(vsym)
      localTyper.atOwner(vsym.owner).typed(vdef)
    }

    private def isAfterPickler: Boolean = {
      global.isAtPhaseAfter(picklerPhase)
    }

    private def picklerRenamingException: Nothing = {
      throw new AssertionError("Renaming is not allowed after Pickler phase")
    }

    private def duplicateException(newName: Name): Nothing = {
      throw new AssertionError(newName + " is already defined in the scope")
    }

    private def isNotAlreadyDefined(sym: Symbol, newName: Name): Boolean = {
      !sym.owner.info.decls.exists(_.name == newName)
    }
  }
}