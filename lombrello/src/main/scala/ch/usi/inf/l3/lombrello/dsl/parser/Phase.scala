package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

abstract class Phase[K, T] {
  def run(input: K): T
}
