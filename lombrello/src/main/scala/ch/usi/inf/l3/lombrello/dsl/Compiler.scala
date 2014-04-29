package ch.usi.inf.l3.lombrello.dsl



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import java.io.File
import parser._



class Compiler extends Trees
  with Parsers {

  lazy val lexer = new Lexer

  lazy val parser = new Parser
}




