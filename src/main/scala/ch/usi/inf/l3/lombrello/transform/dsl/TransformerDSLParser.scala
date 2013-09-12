package ch.usi.inf.l3.lombrello.transform.dsl

import scala.util.parsing.combinator._
import scala.language.postfixOps
import java.io.FileReader
import scala.util.matching.Regex


class TransformerDSLParser extends JavaTokenParsers {
  private def pluginName: Parser[NameTree] = ("name" ~ ":=") ~> stringLiteral ^^ 
		  ((x) => NameTree(x)) | failure("""Name should be defined as follows: name := "name"""")
  
  private def runsAfter: Parser[RunsAfterTree] = (("runsAfter" ~ ":=" ~ "List" ~ "(") 
		  ~> repsep(stringLiteral, ",") <~ ")") ^^ ((x) => RunsAfterTree(x))
    
		      
  
  private def runsRightAfter: Parser[RunsRightAfterTree] = (("runsRightAfter" ~ ":=") ~> 
  		stringLiteral) ^^ ((x) => RunsRightAfterTree(x))
  
  private def runsBefore: Parser[RunsBeforeTree] = (("runsBefore" ~ ":=" ~ "List" ~ "(") 
        ~> repsep(stringLiteral, ",") <~ ")") ^^ ((x) => RunsBeforeTree(x))
  
  
  private def transform(): Parser[List[TransformTree]] = 
    	(("transform" ~ "=" ~ "{") ~> body <~ "}") ^^ ((x) => List(TransformTree(0, x))) | 
    	rep1(((("transform" ~ "(") ~> (""""\d*""".r ^^ ((x) => x.toInt)) <~ (")" ~ "=" ~ "{")) ~ body <~ 
		"}") ^^ ((x) => TransformTree(x._1, x._2))) | 
		failure("At least one transform block should be defined")
  
  private def body: Parser[BodyTree] = (preamble ~ treeMatch) ^^ 
  		((x) => BodyTree(x._1, x._2)) | (treeMatch ^^ ((x) => BodyTree("", x)))
  
  private def preamble: Parser[String] = scalaBlcok
  
  private def treeMatch: Parser[String] = (("tree" ~ "match") ~> 
  		scalaBlcok)
  
  		
  private def scalaBlcok: Parser[String] = ("<--%".r ~> until("""%-->""".r) <~ "%-->".r) | failure("Could not find scala block")
  
  
  // Stolen from stack overflaw :D
  def until(r: Regex): Parser[String] = new Parser[String]{
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = offset
      (r.findFirstMatchIn( source.subSequence(offset, source.length) )) match {
        case Some(matched) => 
          Success(source.subSequence(offset, offset + matched.start).toString, in.drop(matched.start))
        case None => 
          Failure("string matching regex `"+ r +"' expected but `"+ in.first +"' found", in.drop(0))
      }
    }
  }
  		
  def dsl: Parser[List[DSLTree]] = {
    val opt1: Parser[List[DSLTree]] = (runsAfter | runsRightAfter) ~ 
    		runsBefore ^^ ((x) => x._1 :: x._2 :: Nil)
    val pre: Parser[List[DSLTree]] = ((pluginName ~ opt1) ^^ 
        ((x) => x._1 :: x._2) | pluginName ^^ ((x) => List(x)))
    
    val post: Parser[List[DSLTree]] = transform
        
    (pre ~ post) ^^ ((x) => x._1 ++ x._2)
  }
}

object ParseTransformerDSL extends TransformerDSLParser {
  
  def parse(str: String): Boolean = {
    val b = parseAll(dsl, str)
    println(b.toString)
    b.successful
  }
  def main(args: Array[String]): Unit = {
    val src = new FileReader(args(0))
    println(parseAll(dsl, src))
  }
} 

sealed abstract class DSLTree

case class NameTree(name: String) extends DSLTree

case class RunsAfterTree(phases: List[String]) extends DSLTree

case class RunsBeforeTree(phases: List[String]) extends DSLTree

case class RunsRightAfterTree(phase: String) extends DSLTree

case class TransformTree(id: Int, body: BodyTree) extends DSLTree

case class BodyTree(preamble: String, tmatch: String) extends DSLTree