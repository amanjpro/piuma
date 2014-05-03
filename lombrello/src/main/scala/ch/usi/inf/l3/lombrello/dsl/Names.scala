package ch.usi.inf.l3.lombrello.dsl

/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */


object Names {
  val EMPTY_PACKAGE = "<empty>"

  private val scalaKeywords = List("abstract", "class", "do", "extends", "final", 
                            "for", "forSome", "implicit", "lazy", "null",
                            "object", "override", "protected", "return",
                            "sealed", "trait", "type", "val", "var", 
                            "while", "with", "yield")
                          



  def isScala(str: String) = {
    scalaKeywords.contains(str)
  }

}
