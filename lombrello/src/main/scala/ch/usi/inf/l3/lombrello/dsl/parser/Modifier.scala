package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */


class Modifier(mod: Int) {
  import Modifier._

  val isDef = (mod & DEF) == DEF
  val isVariable = (mod & VARIABLE) == VARIABLE
  val isParam = (mod & PARAM) == PARAM
  val isChecker = (mod & CHECKER) == CHECKER
  val isTransformer = (mod & TRANSFORMER) == TRANSFORMER
  val isByName = (mod & BYNAME) == BYNAME


}


object Modifier {

  def apply(mod: Int): Modifier = {
    new Modifier(mod)
  }
  val DEF = 1 << 1
  val VARIABLE = 1 << 2
  val PARAM = 1 << 2
  val CHECKER = 1 << 3
  val TRANSFORMER = 1 << 4
  val BYNAME = 1 << 5
}


