package ch.usi.inf.l3.lombrello.dsl.typechecker

/**
 * @author Amanj Sherwany
 * @date 15 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import scala.annotation.tailrec

trait Namers {self: Compiler =>

  class Namer extends Phase {
    type InputType = self.Tree
    type OutputType = self.Tree
    val name: String = "namer"
    val runsAfter: Option[String] = Some("parser")

    def run(tree: InputType): OutputType = {
      tree match {
        case Program(packages) =>
          combinePackages(packages)
          tree
          // Program(nameTypes(trees))
        case _ => 
          println("HELLO")
          println(tree)
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
  }
}



