package ch.usi.inf.l3.lombrello.dsl

/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */


object Names {

  private var counter = 0
  def uniqueName(prefix: String): String = {
    val nme = s"${prefix}${counter}"
    counter += 1
    nme
  }



  val WILDCARD = "_"
  val TREE_TYPE = "Tree"
  val EMPTY_PACKAGE = "<empty>"
  val TRANSFORMER = "transform"
  val CHECKER = "atPhase"
  val RUNS_BEFORE = "runsBefore"
  val RUNS_RIGHT_AFTER = "runsRightAfter"
  val RUNS_AFTER = "runsAfter"


  val DUMMY_NAME = "dummyMethod"

  private val scalaKeywords = List("abstract", "class", "do", "extends", "final", 
                            "for", "forSome", "implicit", "lazy", "null",
                            "object", "override", "protected", "return",
                            "sealed", "trait", "type", "val", "var", 
                            "while", "with", "yield", "implicitly")
                          



  def isScala(str: String) = {
    scalaKeywords.contains(str)
  }

}
