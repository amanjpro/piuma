package ch.usi.inf.l3.lombrello.dsl.reporter



/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.parser._
import lombrello.dsl.source._

trait Reporters {self: Compiler =>

  val BAD_TOKEN = "Unexpected token is found"

  val SCALA_KEYWORD = "Bad usage of Scala keywords"

  val LINE_FEED = "end of line"

  class Report {
    private var errorBank: List[String] = Nil

    def printErrors(): Unit = {
      errorBank.reverse.foreach(println)
    }

    private def rest(token: tokens.Token): String = {
      token.position match {
        case Some(pos) =>
          rest(pos)
        case _ => ""
      }
    }

    private def rest(pos: Position): String = {
      s"  ${pos.line}\n" +
      s"  ${" " * (pos.col - 1)}^\n" 
    }

    def report(expected: tokens.Token, found: tokens.Token, msg: String): Unit = {
      errorBank = (s"[error] ${found.position.getOrElse("")}: ${msg}\n" +
      s"     found: ${found}\n" +
      s"  expected: ${expected}\n" +
      rest(found)) :: errorBank
      self.errorCounter = self.errorCounter + 1
    }

    def report(found: tokens.Token, msg: String): Unit = {
      errorBank = (s"[error] ${found.position.getOrElse("")}: ${msg}\n" +
      s"     found: ${found}\n" +
      rest(found)) :: errorBank
      self.errorCounter = self.errorCounter + 1
    }


    def report(found: String, pos: Position, msg: String): Unit = {
      errorBank = (s"[error] ${pos}: ${msg}\n" +
      s"     found: ${found}\n" +
      rest(pos)) :: errorBank
      self.errorCounter = self.errorCounter + 1
    }
    
    def report(expected: String, found: String, pos: Position, msg: String): Unit = {
      errorBank = (s"[error] ${pos}: ${msg}\n" +
      s"     found: ${found}\n" +
      s"  expected: ${expected}\n" + 
      rest(pos)) :: errorBank
      self.errorCounter = self.errorCounter + 1
    }
  }
}


