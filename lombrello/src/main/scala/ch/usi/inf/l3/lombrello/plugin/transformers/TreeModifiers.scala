/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.lombrello.plugin.transformers

import ch.usi.inf.l3.lombrello.plugin.TransformerPluginComponent
import scala.reflect.internal.Flags
import ch.usi.inf.l3.lombrello.util.SymbolsUtil

trait TreeModifiersCake {

  editor: TransformerPluginComponent =>

  import editor.plgn._
  trait TreeModifiers {
    self: editor.TransformerComponent =>

    import editor.plgn.global._

    /**
      * Removes parameters from tree of the list of 
      * parameters of a method.
      *
      * @param the method to remove the parameters from
      * @param params the parameters to be removed
      * @param values the values to substitute params
      *
      * @return a new method with params removed
      */
    def removeParams(method: DefDef, params: List[ValDef], 
            values: List[Tree]): DefDef = {
      require(params.size == values.size)
      (params zip values).foldLeft(method)((z, y) => {
        removeParam(z, y._1, y._2)
      })
    }

    /**
      * Removes a parameter from tree of the list of 
      * parameters of a method.
      *
      * @param the method to remove the parameter from
      * @param param the parameter to be removed
      * @param value the value to substitute param
      *
      * @return a new method with param removed
      */
    def removeParam(method: DefDef, param: ValDef, value: Tree): DefDef = {
      require(hasSymbol(param))
      require(
        method.vparamss.flatten.filter(
                  _.symbol.name == param.symbol.name).size == 1)

      val removedParam = method.vparamss.flatten.filter(
                      _.symbol.name == param.symbol.name).head

      val vparamss = method.vparamss.map((x) => {
        x.filter(_.symbol.name != param.symbol.name)
      }).filter(_!=Nil)

      val vparamSyms = vparamss.flatten.map(_.symbol)
        
        
      val mthdInfo = method.tparams.map(_.symbol) match {
        case Nil => MethodType(vparamSyms, method.symbol.info.resultType)
        case tparams => 
          PolyType(tparams, MethodType(vparamSyms, 
                method.symbol.info.resultType))
      }
     val rhs = method.rhs.traverse((x) => {
        x match {
          case Assign(lhs, rhs) if lhs.symbol == removedParam.symbol =>
            throw new Error(
              s"You cannot remove ${lhs}, because it is being re-assigned")
          case t: Ident if t.symbol == removedParam.symbol =>
            value
          case _ => x
        }
      })


      val typedRhs = typed(rhs)



     
      // method.symbol.updateInfo(mthdInfo)


      val updatedMethod = DefDef(method.symbol, vparamss, typedRhs)
      updatedMethod.symbol.updateInfo(mthdInfo)

      updatedMethod.tpt setType localTyper.packedType(typedRhs, method.symbol)
  
      // updatedMethod.symbol.updateInfo(MethodType(
      //   vparamSyms, mthdInfo.resultType))

      localTyper.typed {
        updatedMethod
      }.asInstanceOf[DefDef]
    }

    /**
      * Adds a parameter as the first parameter of the list of 
      * parameters of a constructor.
      *
      * @param pname the name of the parameter to be added
      * @param tpe the type of the parameter to be added
      * @param method the constructor to add the parameter to
      * @param withField a flag to indicate that a field should also
      *        be created. Default is true.
      *
      * @return a new constructor with param added
      */
    def addConstructorParam(pname: String, 
                    tpe: Type, method: DefDef, 
                    withField: Boolean = true): DefDef = {
      require(isConstructor(method))
      val param = mkConstructorParam(method, pname, tpe, withField)
      addParam(param, method)
    }

    /**
      * Adds a parameter as the first parameter of the list of 
      * parameters of a method.
      *
      * @param param the parameter to be added
      * @param the method to add the parameter to
      *
      * @return a new method with param added
      */
    def addParam(param: ValDef, method: DefDef): DefDef = {
      val paramSym = param.symbol

      val vparamss = method.vparamss match {
        case Nil => List(List(param))
        case x :: xs => (param :: x) :: xs
      }
      
      val updatedMethod = treeCopy.DefDef(method, method.mods, method.name, 
                        method.tparams, vparamss, method.tpt, method.rhs)

      val mthdInfo = method.symbol.info.asInstanceOf[MethodType]
  
      updatedMethod.symbol.updateInfo(MethodType(
        paramSym :: mthdInfo.params, mthdInfo.resultType))

      updatedMethod.tpt setType localTyper.packedType(method.rhs, method.symbol)

      localTyper.typed {
        updatedMethod
      }.asInstanceOf[DefDef]

    }


    /**
      * Updates the body of a CaseDef to a rhs.
      *
      * @param tree the CaseDef to be updated
      * @param rhs the new right-hand-side (or value) of 
      *        the CaseDef
      *
      * @return a well-typed CaseDef that is identical to tree,
      *         except that it has rhs as its value.
      */
    def updateBody(tree: CaseDef, rhs: Tree): CaseDef = {
      val cse = treeCopy.CaseDef(tree, tree.pat, tree.guard, rhs)
      localTyper.typed {
        cse 
      }.asInstanceOf[CaseDef]
    }

    /**
     * Updates the cases of a Match to a cases.
      *
      * @param tree the Match to be updated
      * @param cases the new cases of a match
      *
      * @return a well-typed Match that is identical to tree,
      *         except that it has ``cases'' as its cases.
      */
    def updateCases(tree: Match, cases: List[CaseDef]): Match = {
      val mt = treeCopy.Match(tree, tree.selector, cases)
      localTyper.typed {
        mt 
      }.asInstanceOf[Match]
    }
    /**
      * Updates the RHS of a ValDef to a rhs.
      *
      * @param tree the ValDef to be updated
      * @param rhs the new right-hand-side (or value) of 
      *        the ValDef
      *
      * @return a well-typed ValDef that is identical to tree,
      *         except that it has rhs as its value.
      */
    def updateRHS(tree: ValDef, rhs: Tree): ValDef = {
      val nval = treeCopy.ValDef(tree, tree.mods, tree.symbol.name.toTermName, 
                      tree.tpt, rhs).setSymbol(tree.symbol)
      localTyper.typed {
        nval 
      }.asInstanceOf[ValDef]
    }


    /**
      * Add a call to the super constructor to a constructor.
      *
      * @param tree the child constructor, that needs to call the super
      *        constructor.
      * @param args the arguments to the parent constructor
      * @param targs type the arguments to the parent constructor
      *
      * @return a new constructor definition that calls a super constructor.
      */
    def addSuperConstructorCall(tree: DefDef, 
                        targs: List[Tree] = Nil, 
                        args: List[Tree] = Nil): DefDef = {
      require(isConstructor(tree))

      val spr = Select(Super(This(tree.symbol.owner), tpnme.EMPTY), 
                        nme.CONSTRUCTOR)
      val app = mkApply(spr, args, targs) 

      val ths = This(tree.symbol.owner)
      ths.setType(tree.symbol.owner.tpe)
      app.substituteThis(tree.symbol.owner, ths)

      val ocbody = tree.rhs match {
        case block @ Block(stats, ret) => 
          stats match {
            case Apply(Select(_, nme.CONSTRUCTOR), _) :: xs =>
              mkBlock(app :: xs, ret)
            case xs =>
              mkBlock(app :: xs, ret)
          }
        case _ =>
          mkBlock(List(app, mkUnitLiteral))
      }



      updateRHS(tree, ocbody)
    }


    /**
      * Updates the RHS of a method to a rhs.
      *
      * @param tree the method to be updated
      * @param rhs the new right-hand-side (or value) of 
      *        the method
      *
      * @return a well-typed DefDef that is identical to tree,
      *         except that it has rhs as its value.
      */
    def updateRHS(tree: DefDef, rhs: Tree): DefDef = {
      val ndef = treeCopy.DefDef(tree, tree.mods, tree.symbol.name.toTermName,
                  tree.tparams, tree.vparamss, tree.tpt, 
                  rhs).setSymbol(tree.symbol)
      localTyper.typed {
        ndef 
      }.asInstanceOf[DefDef]
    }


    /**
      * Updates the body of a class to the newBody.
      *
      * @param tree the class to be updated
      * @param newBody the new body of the class
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it has newBody as its body.
      */
    def updateBody(tree: ClassDef, newBody: List[Tree]): ClassDef = {
      val nclazz = ClassDef(tree.symbol, 
                  Template(tree.impl.parents, 
                        tree.impl.self,
                        newBody))
      localTyper.typed {
        nclazz 
      }.asInstanceOf[ClassDef]
    }


    /**
      * Updates the body of a module to the newBody.
      *
      * @param tree the module to be updated
      * @param newBody the new body of the module
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it has newBody as its body.
      */
    def updateBody(tree: ModuleDef, newBody: List[Tree]): ModuleDef = {
      val nclazz = treeCopy.ModuleDef(tree, tree.mods, 
                  tree.symbol.name.toTermName, 
                  treeCopy.Template(tree.impl, tree.impl.parents,
                             tree.impl.self,
                             newBody))
      localTyper.typed {
        nclazz 
      }.asInstanceOf[ModuleDef]
    }

    /**
      * Updates the body of a module/class to the newBody.
      *
      * @param tree the module/class to be updated
      * @param newBody the new body of the module/class
      *
      * @return a well-typed ModuleDef/ClassDef that is identical to tree,
      *         except that it has newBody as its body.
      */

    def updateBody(tree: ImplDef, newBody: List[Tree]): ImplDef = {
      tree match {
        case x: ModuleDef => updateBody(x, newBody)
        case x: ClassDef => updateBody(x, newBody)
      }
    }


    /**
      * Updates the stats of a package to the newStats.
      *
      * @param tree the package to be updated
      * @param newStats the new stats of the package
      *
      * @return a well-typed PackageDef that is identical to tree,
      *         except that it has newStats as its body.
      */

    def updateStats(tree: PackageDef, newStats: List[Tree]): PackageDef = {
      typed {treeCopy.PackageDef(tree, tree.pid,
                newStats)}.asInstanceOf[PackageDef]
    }


    /**
      * Adds a member to a class
      *
      * @param tree the class to be updated
      * @param member the member to be added to the class
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it has an extra memmber, namely ``member''
      */
    def addMember(tree: ClassDef, member: Tree): ClassDef = {
      updateBody(tree, tree.impl.body ++ List(member))
    }

    /**
      * Adds a member to a module
      *
      * @param tree the module to be updated
      * @param member the member to be added to the module
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it has an extra memmber, namely ``member''
      */
    def addMember(tree: ModuleDef, member: Tree): ModuleDef = {
      updateBody(tree, tree.impl.body ++ List(member))
    }

    /**
      * Adds a member to a module/class
      *
      * @param tree the module/class to be updated
      * @param member the member to be added to the module/class
      *
      * @return a well-typed ModuleDef/ClassDef that is identical to tree, 
      *         except that it has an extra memmber, namely ``member''
      */
    def addMember(tree: ImplDef, member: Tree): ImplDef = {
      tree match {
        case t: ModuleDef => addMember(t, member)
        case t: ClassDef => addMember(t, member)
      }
    }

    /**
      * Adds a member to a package
      *
      * @param tree the package to be updated
      * @param member the member to be added to the package
      *
      * @return a well-typed PackageDef that is identical to tree,
      *         except that it has an extra memmber, namely ``member''
      */
    def addMember(tree: PackageDef, member: Tree): PackageDef = {
      updateStats(tree, tree.stats ++ List(member))
    }


    /**
      * Removes a member from a class
      *
      * @param tree the class to be updated
      * @param p a predicate for removing members
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it doesn't have members that satisfy p
      */
    def removeMember(tree: ClassDef, p: Tree => Boolean): ClassDef = {
      updateBody(tree, tree.impl.body.filter(!p(_)))
    }

    /**
      * Removes a member from a module
      *
      * @param tree the module to be updated
      * @param p a predicate for removing members
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it doesn't have members that satisfy p
      */
    def removeMember(tree: ModuleDef, p: Tree => Boolean): ModuleDef = {
      updateBody(tree, tree.impl.body.filter(!p(_)))
    }

    /**
      * Removes a member from a module/class
      *
      * @param tree the module/class to be updated
      * @param p a predicate for removing members
      *
      * @return a well-typed ModuleDef/ClassDef that is identical to tree, 
      *         except that it doesn't have members that satisfy p
      */
    def removeMember(tree: ImplDef, p: Tree => Boolean): ImplDef = {
      tree match {
        case t: ModuleDef => removeMember(t, p)
        case t: ClassDef => removeMember(t, p)
      }
    }

    /**
      * Removes a member from a package
      *
      * @param tree the package to be updated
      * @param p a predicate for removing members
      *
      * @return a well-typed PackageDef that is identical to tree,
      *         except that it doesn't have members that satisfy p
      */
    def removeMember(tree: PackageDef, p: Tree => Boolean): PackageDef = {
      updateStats(tree, tree.stats.filter(!p(_)))
    }

    /**
      * Removes a member from a class
      *
      * @param tree the class to be updated
      * @param member the member to be deleted from the class
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it deosn't have ``member''
      */
    def removeMember(tree: ClassDef, member: Tree): ClassDef = {
      updateBody(tree, tree.impl.body.filter(_ != member))
    }

    /**
      * Removes a member from a module
      *
      * @param tree the module to be updated
      * @param member the member to be removed from the module
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it doesn't have ``member''
      */
    def removeMember(tree: ModuleDef, member: Tree): ModuleDef = {
      updateBody(tree, tree.impl.body.filter(_ != member))
    }

    /**
      * Removes a member from a module/class
      *
      * @param tree the module/class to be updated
      * @param member the member to be removed from the module/class
      *
      * @return a well-typed ModuleDef/ClassDef that is identical to tree, 
      *         except that it doesn't have ``member''
      */
    def removeMember(tree: ImplDef, member: Tree): ImplDef = {
      tree match {
        case t: ModuleDef => removeMember(t, member)
        case t: ClassDef => removeMember(t, member)
      }
    }

    /**
      * Removes a member from a package
      *
      * @param tree the package to be updated
      * @param member the member to be removed from the package
      *
      * @return a well-typed PackageDef that is identical to tree,
      *         except that it doesn't have ``member''
      */
    def removeMember(tree: PackageDef, member: Tree): PackageDef = {
      updateStats(tree, tree.stats.filter(_ != member))
    }


    /**
      * Modifies the list of parents of a class
      *
      * @param tree the class to be updated
      * @param parents the list to be set as the parents of the class
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it has ``parents'' as its parents
      */
    def updateParents(tree: ClassDef, parents: List[Type]): ClassDef = {
      val parentsTree = parents.map((x) => typed {TypeTree(x)})
      val ntpe = ClassInfoType(parents, tree.symbol.info.decls, tree.symbol)
      tree.symbol.updateInfo(ntpe)
      typed {
        treeCopy.ClassDef(tree, tree.mods, tree.name, tree.tparams, 
            treeCopy.Template(tree.impl, parentsTree, tree.impl.self,
                              tree.impl.body))
      }.asInstanceOf[ClassDef]
    }


    /**
      * Modifies the list of parents of a module
      *
      * @param tree the module to be updated
      * @param parents the list to be set as the parents of the module
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it has ``parents'' as its parents
      */
    def updateParents(tree: ModuleDef, parents: List[Type]): ModuleDef = {
      val symbol = tree.symbol.moduleClass
      val parentsTree = parents.map((x) => typed {TypeTree(x)})
      val ntpe = ClassInfoType(parents, symbol.info.decls, symbol)
      symbol.setInfo(ntpe)
      typed {
        treeCopy.ModuleDef(tree, tree.mods, tree.name, 
          treeCopy.Template(tree.impl, parentsTree, tree.impl.self,
            tree.impl.body))
      }.asInstanceOf[ModuleDef]
    }


    /**
      * Modifies the list of parents of a class/module
      *
      * @param tree the class/module to be updated
      * @param parents the list to be set as the parents of the class/module
      *
      * @return a well-typed ClassDef/ModuleDef that is identical to tree,
      *         except that it has ``parents'' as its parents
      */
    def updateParents(tree: ImplDef, parents: List[Type]): ImplDef = {
      tree match {
        case x: ModuleDef => updateParents(x, parents)
        case x: ClassDef => updateParents(x, parents)
      }
    }



    /**
      * Adds a parent to the list of parents of a class
      *
      * @param tree the class to be updated
      * @param parent the parent to be added to the class
      *
      * @return a well-typed ClassDef that is identical to tree,
      *         except that it has ``parent'' in its parents
      */
    def addParent(tree: ClassDef, parent: Type): ClassDef = {
      updateParents(tree, tree.symbol.info.parents ++ List(parent))
    }


    /**
      * Adds a parent to the list of parents of a module
      *
      * @param tree the module to be updated
      * @param parent the parent to be added to the module
      *
      * @return a well-typed ModuleDef that is identical to tree,
      *         except that it has ``parent'' in its parents
      */
    def addParent(tree: ModuleDef, parent: Type): ModuleDef = {
      updateParents(tree, tree.symbol.info.parents ++ List(parent))
    }


    /**
      * Adds a parent to the list of parents of a class/module
      *
      * @param tree the class/module to be updated
      * @param parent the parent to be added to the class/module
      *
      * @return a well-typed ClassDef/ModuleDef that is identical to tree,
      *         except that it has ``parent'' in its parents
      */
    def addParent(tree: ImplDef, parent: Type): ImplDef = {
      tree match {
        case x: ModuleDef => 
          updateParents(tree, x.symbol.info.parents ++ List(parent))
        case x: ClassDef => 
          updateParents(tree, x.symbol.info.parents ++ List(parent))
      }
    }
  }
}


