package ch.usi.inf.l3.lombrello.dsl.reporter



/**
 * @author Amanj Sherwany
 * @date 2 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._

trait Reporters {self: Compiler =>

  val BAD_TOKEN = "Unexpected token is found"


  class Report {
    var errorBank: List[String] = Nil

    def report(expected: tokens.Token, found: tokens.Token, msg: String): Unit = {
      (s"[error] ${found.position.getOrElse("")}: ${msg}\n" +
      s"     found: ${found}\n" +
      s"  expected: ${expected}\n") :: errorBank
      self.errorCounter = self.errorCounter + 1
    }

    def report(found: tokens.Token, msg: String): Unit = {
      (s"[error] ${found.position.getOrElse("")}: ${msg}\n" +
      s"     found: ${found}\n") :: errorBank
      self.errorCounter = self.errorCounter + 1
    }
  }
}


