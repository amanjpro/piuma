/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.mina.eval

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.language.implicitConversions
import ch.usi.inf.l3.mina._

@treeTransformer("mina-finalizer") class HPEFinalizer {

  plugin HPE

  rightAfter(specializer)
  before(List("superaccessors"))




  private def memequal(m1: Tree, m2: Tree): Boolean = {
    m1.symbol.name == m2.symbol.name &&
      m1.symbol.tpe =:= m2.symbol.tpe
  }

  private def doesNotHaveThisMember(trees: List[Tree], m: Tree): Boolean = {
    trees.filter(memequal(_, m)) == Nil
  }

  def transform(tree: Tree): Tree = {

    def typeTree(tree: Tree): Tree = {
//        if (tree.symbol != null && tree.symbol != NoSymbol)
//          atOwner(tree.symbol.owner) { localTyper.typed { tree } }
//        else
      localTyper.typed { tree }
    }

    val t: Tree = tree match {
      case pkg @ PackageDef(pkname, stats) =>
        val morphs = for (stat <- stats) yield {
          
          stat match {
            case x: ImplDef =>
              val tree = digraph.getClassRepr(x.symbol.tpe) match {
                case Some(clazz) =>
                  val sclazz = clazz.tree
                  var obody = x.impl.body
                  var tail = sclazz.impl.body
                  var added = false
                  while(tail != Nil) {
                    val head = tail.head
                    if(doesNotHaveThisMember(obody, head)) {
                      val tnm = typeTree(head)
                      obody = typeTree(tnm) :: obody 
                      added = true
                    }
                    tail = tail.tail
                  }
                  
                  val nbody = obody
                 
                  (added, x) match {
                    case (true, y: ModuleDef) =>
                      
                      
                      typeTree(treeCopy.ModuleDef(y, y.mods, y.name,
                        treeCopy.Template(y.impl, y.impl.parents, y.impl.self,
                          nbody)))
                            
                    case (true, y: ClassDef) =>
                      typeTree(treeCopy.ClassDef(y, y.mods, y.name, y.tparams,
                        treeCopy.Template(y.impl, y.impl.parents, y.impl.self,
                          nbody)))
                    case _ => x
                  }
                case None => x
              }
              tree.symbol.isClass match {
                case true => 
                  val classes = tree :: classBank.getAllMorphs(x.symbol.tpe)
                  val comp = digraph.getCompanion(stat.tpe)
                  comp match {
                    case Nil => classes
                    case x :: _ => 
                      classes.contains(comp) match {
                      case true => classes
                      case _ => classes ++ comp
                    }
                  }
                case false => List(tree)
              }
                             

            case x => List(x)
          }
          
        }
//          val pkgsymb = pkg.symbol
        val flatMorphs = morphs.flatten
        val newpkg = treeCopy.PackageDef(pkg, pkname, flatMorphs) //setSymbol (pkgsymb)

        //          localTyper.typed(newpkg)
        //          localTyper.typed { newpkg }

        typeTree(newpkg)
      case y => super.transform(y)
    }
    t
    //      tree
  }
}
