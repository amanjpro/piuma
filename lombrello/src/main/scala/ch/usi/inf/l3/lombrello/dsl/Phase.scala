package ch.usi.inf.l3.lombrello.dsl

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

abstract class Phase[K, T] {
  val name: String
  val runsAfter: Option[String]
  def run(input: K): T
}
