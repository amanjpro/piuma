package ch.usi.inf.l3.lombrello.dsl.typechecker

/**
 * @author Amanj Sherwany
 * @date 9 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._

trait Typers {self: Compiler =>

  class Typer extends Phase {
    type InputType = self.Tree
    type OutputType = self.Tree
    val name: String = "typer"
    val runsAfter: Option[String] = Some("namer")

    def run(tree: InputType): OutputType = {
      tree
    }
  }

}



// TODO: You can type check the dsl using scala's reflect api
// val ms = ru.runtimeMirror(myClassLoader).staticClass(
//                                         "model.Model").typeSignature.members
// ms.foreach((x: reflect.runtime.universe.Symbol) => println(x.typeSignature))
