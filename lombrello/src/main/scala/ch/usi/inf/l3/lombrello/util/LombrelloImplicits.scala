/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.util


import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent

trait LombrelloImplicitsCake {
  implicits: TransformerPluginComponent =>
  trait LombrelloImplicits {
    self: implicits.TransformerComponent =>

    import implicits.plgn._
    import implicits.plgn.global._

    implicit class AugmentedTree(tree: Tree) {
      def traverseIf(f: Tree => Tree, p: Tree => Boolean): Tree = {
        self.traverseIf(tree, f, p)
      }

      def traverse(f: Tree => Tree): Tree = {
        self.traverse(tree, f)
      }
    }

    implicit class AugmentedImplDef(impl: ImplDef) {
      def updateBody(body: List[Tree]): ImplDef = {
        self.updateBody(impl, body)
      }
      def duplicate(name: Name, owner: Symbol): ImplDef = {
        self.duplicate(impl, name, owner).asInstanceOf[ImplDef]
      }

      def duplicate(name: Name): ImplDef = {
        self.duplicate(impl, name).asInstanceOf[ImplDef]
      }

      def addMember(m: Tree): ImplDef = {
        self.addMember(impl, m)
      }

      def removeMember(m: Tree): ImplDef = {
        self.removeMember(impl, m)
      }

      def removeMember(p: Tree => Boolean): ImplDef = {
        self.removeMember(impl, p)
      }

      def updateParents(parents: List[Tree]): ImplDef = {
        self.updateParents(impl, parents)
      }

    }

    implicit class AugmentedClassDef(clazz: ClassDef) {
      def updateBody(body: List[Tree]): ClassDef = {
        self.updateBody(clazz, body)
      }
      def addMember(m: Tree): ClassDef = {
        self.addMember(clazz, m)
      }

      def duplicate(name: TypeName, owner: Symbol): ClassDef = {
        self.duplicate(clazz, name, owner).asInstanceOf[ClassDef]
      }

      def duplicate(name: TypeName): ClassDef = {
        self.duplicate(clazz, name).asInstanceOf[ClassDef]
      }

      def removeMember(m: Tree): ClassDef = {
        self.removeMember(clazz, m)
      }

      def removeMember(p: Tree => Boolean): ClassDef = {
        self.removeMember(clazz, p)
      }

      def updateParents(parents: List[Tree]): ClassDef = {
        self.updateParents(clazz, parents)
      }

      def mkConstructor: DefDef = {
        self.mkConstructor(clazz)
      }

      def mkCompanionObject: ModuleDef = {
        self.mkCompanionObject(clazz.symbol.asClass)
      }
    }

    implicit class AugmentedModuleDef(module: ModuleDef) {
      def updateBody(body: List[Tree]): ModuleDef = {
        self.updateBody(module, body)
      }
      def addMember(m: Tree): ModuleDef = {
        self.addMember(module, m)
      }

      def duplicate(name: TermName, owner: Symbol): ModuleDef = {
        self.duplicate(module, name, owner).asInstanceOf[ModuleDef]
      }

      def duplicate(name: TermName): ModuleDef = {
        self.duplicate(module, name).asInstanceOf[ModuleDef]
      }



      def removeMember(m: Tree): ModuleDef = {
        self.removeMember(module, m)
      }

      def removeMember(p: Tree => Boolean): ModuleDef = {
        self.removeMember(module, p)
      }

      def updateParents(parents: List[Tree]): ModuleDef = {
        self.updateParents(module, parents)
      }
    }
  
    implicit class AugmentedDefDef(mthd: DefDef) {
      def updateRHS(rhs: Tree): DefDef = {
        self.updateRHS(mthd, rhs)
      }

      def duplicate(name: TermName, owner: Symbol): DefDef = {
        self.duplicate(mthd, name, owner).asInstanceOf[DefDef]
      }

      def duplicate(name: TermName): DefDef = {
        self.duplicate(mthd, name).asInstanceOf[DefDef]
      }

      def removeParam(param: ValDef, value: Tree): DefDef = {
        self.removeParam(mthd, param, value)
      }
 
      def removeParams(params: List[ValDef], values: List[Tree]): DefDef = {
        self.removeParams(mthd, params, values)
      }


      def addSuperConstructorCall(targs: List[Tree], args: List[Tree]): DefDef = {
        self.addSuperConstructorCall(mthd, targs, args)
      }

      def addSuperConstructorCall(arguments: List[Tree]): DefDef = {
        self.addSuperConstructorCall(mthd, args = arguments)
      }

      def addSuperConstructorCall: DefDef = {
        self.addSuperConstructorCall(mthd)
      }

    }
    implicit class AugmentedValDef(v: ValDef) {
      def updateRHS(rhs: Tree): ValDef = {
        self.updateRHS(v, rhs)
      }
    }

    implicit class AugmentedCaseDef(cse: CaseDef) {
      def updateBody(rhs: Tree): CaseDef = {
        self.updateBody(cse, rhs)
      }
    }

    implicit class AugmentedMatch(mt: Match) {
      def updateCases(cases: List[CaseDef]): Match = {
        self.updateCases(mt, cases)
      }
    }
  }
}


