package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */


class Modifier(mod: Int) {
  import Modifier._

  val isPublic = (mod & PUBLIC) == PUBLIC
  val isPrivate = (mod & PRIVATE) == PRIVATE
  val isParam = (mod & PARAM) == PARAM
  val isChecker = (mod & CHECKER) == CHECKER
  val isTransformer = (mod & TRANSFORMER) == TRANSFORMER

}


object Modifier {

  def apply(mod: Int): Modifier = {
    new Modifier(mod)
  }
  val PUBLIC = 1 << 1
  val PRIVATE = 1 << 2
  val PARAM = 1 << 3
  val CHECKER = 1 << 4
  val TRANSFORMER = 1 << 5
}


