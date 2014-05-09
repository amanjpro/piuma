package ch.usi.inf.l3.lombrello.dsl.codegen

/**
 * @author Amanj Sherwany
 * @date 9 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._

trait Finalizers {self: Compiler =>

  class Finalizer extends Phase {
    type InputType = List[CompiledCode]
    type OutputType = Int

    val name = "finalizer"
    val runsAfter = Some("codegen")

    def run(gencodes: InputType): OutputType = {
      gencodes.foldLeft(0)((z, y) => {
        y.writeToFile + z
      })
    }
  }
}



