package ch.usi.inf.l3.lombrello.dsl.typechecker

/**
 * @author Amanj Sherwany
 * @date 15 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.source._
import scala.annotation.tailrec

trait Namers {self: Compiler =>

  class Namer extends Phase {
    type InputType = self.Tree
    type OutputType = self.Tree
    val name: String = "namer"
    val runsAfter: Option[String] = Some("refcheck")

    def run(tree: InputType): OutputType = {
      tree match {
        case Program(packages) =>
          val combinedPackages = combinePackages(packages)
          val organizedPackages = organizePackages(combinedPackages)
          // val namedPackages = packagesNamer(combinedPackages)
          Program(packages)
        case _ => 
          tree
      }
    }

    private def sameQuals(pkg1: Tree, 
        pkg2: Tree): Boolean = {
      (pkg1, pkg2) match {
        case (Ident(n1, _, _), Ident(n2, _, _)) => n1 == n2
        case (Select(q1, id1, _, _), Select(q2, id2, _, _)) =>
          sameQuals(id1, id2) && sameQuals(q1, q2)
        case _ => false
      }
    }

    @tailrec private def findSimilarPackages(packages: List[PackageDef], 
        pkg: PackageDef, found: List[PackageDef], 
        rest: List[PackageDef]): (PackageDef, List[PackageDef]) = { 

      packages match {
        case Nil => 
          val trees = found.foldLeft(Nil: List[Tree])((z, y) => {
            z ++ y.trees
          })
          (pkg.copy(trees = trees), rest.reverse)
        case x :: xs if sameQuals(x.pid, pkg.pid) =>
          findSimilarPackages(xs, pkg, x :: found, rest)
        case x :: xs =>
          findSimilarPackages(xs, pkg, found, x :: rest)
      }
    }


    @tailrec private def combinePackages(packages: List[PackageDef], 
          computed: List[PackageDef] = Nil): List[PackageDef] = {
      packages match {
        case Nil => computed.reverse
        case x :: xs =>
          val (r, rest) = findSimilarPackages(xs, x, Nil, Nil)
          combinePackages(rest, r :: computed)
      }
    }


    // def findOwnerPackages(pkgs: Map[String, PackageDef], 
    //       packages: List[PackageDef],
    //       mainTree: PackageTree): 
    //             (Map[String, PackageDef], List[PackageDef]) = {

    //   packages match {
    //     case x :: xs =>
    //       val innerPackageIds = x.pidString.split('.')
    //       pkgs
    //     case Nil =>
    //   }
    // }

    private def organizePackages(packages: List[PackageDef]): PackageDef = {
      val pTree = new PackageTree(Names.ROOT_PACKAGE)

      packages.foreach((x) => pTree.add(x.pidString, x))

      pTree.traverse
    }
    private def topLevelNamer(tree: Tree, owner: Symbol): Tree = {
      tree match {
        case pkg @ PackageDef(pid, trees, pos, _) =>
          val sym = PackageSymbol(pkg.pidString, owner)
          owner.add(sym)
          val trees2 = trees.map((x) => topLevelNamer(x, sym))
          PackageDef(pid, trees2, pos, sym) 
        case phase @ PhaseDef(name, _, _, _, _, _, _) =>
          // val sym = TypeSymbol(name, )
          phase
        case _ => tree
      }
    }


    private def typeNamer(owner: Symbol, tree: Tree): Tree = {
      tree
      // tree match {
        // case 
      // }
    }
  }


  private class PackageTree private (val value: String, 
            private val pkg: PackageDef) { 
    


    def this(v: String) = {
      this(v, PackageDef(Ident(Names.ROOT_PACKAGE, Position()), Nil, Position()))
    }

    private var children: List[PackageTree] = Nil

    def traverse: PackageDef = {
      @tailrec def helper(kids: List[PackageTree],
            computed: PackageDef): PackageDef = {
        kids match {
          case Nil => computed
          case x :: xs => 
            val l = x.traverse
            val temp = computed.copy(trees = l :: computed.trees)

            helper(xs, temp)
        }
      }

      helper(children, pkg)
    }


    def print(): Unit = {
      print(children)
    }

    def print(trees: List[PackageTree]): Unit = {
      trees match {
        case x :: xs =>
          println(x.pkg.pidString)
          x.print
          print(xs)
        case Nil => ()
      }
    }

    def add(elem: String, pkg: PackageDef): Unit = {
      add(elem.split('.').toList, pkg, "")
    }

    private def add(elem: List[String], pkg: PackageDef,
            path: String): Unit = {
      elem match {
        case x :: xs if !children.exists(_.value == x) => 
          val pth = if(path == "") x else s"${path}.${x}"
          val temp = xs match {
            case Nil => new PackageTree(x, pkg)
            case _ => 
              val pid = Ident(pth, Position())
              val p = PackageDef(pid, Nil, Position())
              new PackageTree(x, p)
          }
          temp.add(xs, pkg, pth)
          children = temp :: children
        case x :: xs =>
          val pth = if(path == "") x else s"${path}.${x}"
          children.filter(_.value == x) match {
            case c :: Nil => c.add(xs, pkg, pth)
            case _ => ()
          }
        case _ => ()
      }
    }
  }


  
}
