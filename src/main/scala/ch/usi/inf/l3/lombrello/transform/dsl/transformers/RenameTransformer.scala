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
  // TODO: we have no support for inheritance yet: see bug #2
  renamer: TransformerPluginComponent =>
  trait RenameTransformer {
    self: renamer.TransformerComponent =>

    import renamer.global._

    /**
     * A method to check the validity of a rename. Every rename is validate iff:
     * 1- The tree to be renamed is one of the following:
     *    (ClassDef, ValDef, DefDef, ModuleDef, Bind, PackageDef and TypeDef)
     * 2- The renaming does not cause any conflicts in the current scope
     * 3- The renaming does not cause any conflicts in the scopes belonging to the
     *    child classes of the current class.
     * 4- Most importantly, the current phase is running before pickler (see bug #1)
     *
     * @see RenameTransformer#rename(ClassDef, TypeName): Tree
     * @see rename(ValDef, TermName): Tree
     * @see rename(DefDef, TermName): Tree
     * @see rename(ModuleDef, TermName): Tree
     * @see rename(Bind, TermName): Tree
     * @see rename(PackageDef, TermName): Tree
     * @see rename(TypeDef, TermName): Tree
     *
     * @param tree the tree to be renamed
     * @param newName the new name
     *
     * @return true if renaming is possible, false otherwise
     */
    final def canRename(tree: Tree, newName: Name): Boolean = {
      canRename(tree, newName, !isAfterPickler)
    }

    /**
     * Renames the given class, a rename cannot be performed if the current phase
     * is running after pickler. Renaming a class also renames its companion
     * object, which means both the new class name and companion object name
     * must be unique in the current scope, in order for the renaming to succeed.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the class to be renamed
     * @param newName the new name
     *
     * @return the class tree after renaming
     */
    final def rename(tree: ClassDef, newName: TypeName): Tree = {
      //TODO: What about inner classes? what if renaming caused them to be duplicate in one of their child classes?

      val csym = tree.symbol
      val msym = csym.companionModule

      renameSanityCheck(tree, newName)

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
     * Renames the given {{object}}, a rename cannot be performed if the current phase
     * is running after pickler. Renaming an object also renames its companion
     * class, which means both the new class name and companion object name
     * must be unique in the current scope, in order for the renaming to succeed.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the object to be renamed
     * @param newName the new name
     *
     * @return the object tree after renaming
     */
    final def rename(tree: ModuleDef, newName: TermName): Tree = {
      val msym = tree.symbol
      val msymClass = msym.moduleClass
      val mName = newName
      msymClass.name = mName
      msym.name = mName
      
      val csym = msym.companionClass

      renameSanityCheck(tree, newName)

      csym match {
        case NoSymbol => ()
        case _ =>
          val mName = newName.toTypeName
          csym.name = mName
      }

      val mdef = tree.copy(name = newName).setSymbol(msym)
      localTyper.atOwner(msym.owner).typed(mdef)
    }

    /**
     * Renames the given method, a rename cannot be performed if the current phase
     * is running after pickler. If the current method is an overridable method
     * then, renaming also renames the method in the known subclasses of
     * this method's class, which means an additional check is performed
     * in order to make sure this renaming won't introduce any duplicates
     * in the subclasses too.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the method to be renamed
     * @param newName the new name
     *
     * @return the method tree after renaming
     */
    final def rename(tree: DefDef, newName: TermName): Tree = {
      val tsym = tree.symbol

      renameSanityCheck(tree, newName)

      tsym.name = newName

      val ttree = tree.copy(name = newName).setSymbol(tsym)
      localTyper.atOwner(tsym.owner).typed(ttree)
    }

    /**
     * Renames the given package, a rename cannot be performed if the current phase
     * is running after pickler. In order to make sure this renaming won't
     * introduce any duplicates in the subclasses too.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the package to be renamed
     * @param newName the new name
     *
     * @return the package tree after renaming
     */
    final def rename(tree: PackageDef, newName: TermName): Tree = {
      val tsym = tree.symbol

      renameSanityCheck(tree, newName)

      tsym.name = newName

      // TODO: double check this
      val ttree = tree.copy(pid = Ident(tsym))
      localTyper.atOwner(tsym.owner).typed(ttree)
    }

    /**
     * Renames the given bind, a rename cannot be performed if the current phase
     * is running after pickler. In order to make sure this renaming won't
     * introduce any duplicates in the subclasses too.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the bind to be renamed
     * @param newName the new name
     *
     * @return the bind tree after renaming
     */
    final def rename(tree: Bind, newName: TermName): Tree = {
      val tsym = tree.symbol

      renameSanityCheck(tree, newName)

      tsym.name = newName

      val ttree = tree.copy(name = newName).setSymbol(tsym)
      localTyper.atOwner(tsym.owner).typed(ttree)
    }

    /**
     * Renames the given type, a rename cannot be performed if the current phase
     * is running after pickler. If the current type is an overridable type
     * then, renaming also renames the type in the known subclasses of
     * this type's class, which means an additional check is performed
     * in order to make sure this renaming won't introduce any duplicates
     * in the subclasses too.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the type to be renamed
     * @param newName the new name
     *
     * @return the type tree after renaming
     */
    final def rename(tree: TypeDef, newName: TypeName): Tree = {
      val tsym = tree.symbol

      renameSanityCheck(tree, newName)

      tsym.name = newName

      val ttree = tree.copy(name = newName).setSymbol(tsym)
      localTyper.atOwner(tsym.owner).typed(ttree)
    }

    /**
     * Renames the given variable, a rename cannot be performed if the current
     * phase is running after pickler. Renaming a variable also renames its
     * setters and getters, which means the new variable, getter and setter
     * names must be unique in the current scope, in order for the renaming to
     * succeed. If the current variable is actually an overridable filed then,
     * renaming also renames the field in the known subclasses of the this
     * field's class, which means an additional check is performed in order
     * to make sure this renaming won't introduce any duplicates in the
     * subclasses too.
     *
     * @see canRename(Tree, Name)
     *
     * @throws AssertionError exception if renaming is not possible
     * @param tree the variable to be renamed
     * @param newName the new name
     *
     * @return the variable tree after renaming
     */
    final def rename(tree: ValDef, newName: TermName): Tree = {
      val vsym = tree.symbol
      val vname = nme.getterToLocal(newName)
      val setterName = nme.getterToSetter(newName)
      renameSanityCheck(tree, newName)

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

    /**
     * A method to check the validity of a rename. Every rename is validate iff:
     * 1- The tree to be renamed is one of the following:
     *    (ClassDef, ValDef, DefDef, ModuleDef, Bind, PackageDef and TypeDef)
     * 2- The renaming does not cause any conflicts in the current scope
     * 3- The renaming does not cause any conflicts in the scopes belonging to the
     *    child classes of the current class.
     * 4- Most importantly, beforePickler should be set true
     *
     * @see rename(ClassDef, TypeName): Tree
     * @see rename(ValDef, TermName): Tree
     * @see rename(DefDef, TermName): Tree
     * @see rename(ModuleDef, TermName): Tree
     * @see rename(Bind, TermName): Tree
     * @see rename(PackageDef, TermName): Tree
     * @see rename(TypeDef, TermName): Tree
     *
     * @param tree the tree to be renamed
     * @param newName the new name
     * @param beforePickler a flag to tell the method whether it is after Pickler
     * phase or not
     *
     * @return true if renaming is possible, false otherwise
     */
    private def canRename(tree: Tree, newName: Name, beforePickler: Boolean): Boolean = {
      // Renaming is not allowed after Pickler phase. see bug #1
      lazy val isValid: Boolean = tree match {
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
          val noDuplicate = isNotAlreadyDefined(vsym, vname)
          valid && noDuplicate
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
          validModule && isNotAlreadyDefined(csym, newName)
        case x: ModuleDef =>
          val msym = x.symbol
          val msymClass = msym.moduleClass

          val validClass = msym.companionClass match {
            case NoSymbol => true
            case csym => isNotAlreadyDefined(csym, newName.toTypeName)
          }
          val validModule = isNotAlreadyDefined(msym, newName)
          validModule && validClass
        case DefDef(_, _, _, _, _, _) |
          TypeDef(_, _, _, _) | Bind(_, _) | PackageDef(_, _) =>
          isNotAlreadyDefined(tree.symbol, newName)
        case _ => false
      }

      beforePickler && isValid
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

    private def renameSanityCheck(tree: Tree, newName: Name): Unit = {

      if (!canRename(tree, newName)) {
        if (isAfterPickler) {
          picklerRenamingException
        } else {
          duplicateException(newName)
        }
      }
    }
  }
}