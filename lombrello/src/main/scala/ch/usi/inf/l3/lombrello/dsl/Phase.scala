package ch.usi.inf.l3.lombrello.dsl

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */


abstract class Phase {
  type InputType
  type OutputType
  type ReifiedInput = Manifest[InputType]

  val name: String
  val runsAfter: Option[String]
  def run(input: InputType): OutputType
}


