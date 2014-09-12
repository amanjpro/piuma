/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.mina.eval

import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.language.implicitConversions
import ch.usi.inf.l3.mina._
import store._



/**
  * This plugin has three compiler phases:
  * 1- In the first phase, we draw a class diagram and find the members of
  *    each class
  * 2- In the second phase we try to collect as much information as possible
  *    about which method needs to be specialized.
  * 3- In the third phase, we specialize the methods that need to be
  *    specialized, and we change the method calls from unspecialized methods
  *    to their specialized versions.
  */
@plugin(HPEFinder,
        HPEFinalizer,
        HPESpecializer) class HPE
  extends HPEClassWrapper 
  with HPEEnvironmentWrapper {
  
  var env = Environment.empty
  var closed = false
  val digraph = new ClassDigraph
  val classBank = new ClassBank
  
  
  val name = "mina"
  val beforeFinder = "typer"


  val bfr = "patmat"
  val finderPhase = s"mina-finder"
  val specializer = s"mina-specializer"
  val finalizer = s"mina-finalizer"
  val aftr = "superaccessors"
  
  describe("""|This is a partial evaluator plugin based on Hybrid 
    |Partial Evaluation by W. Cook and A. Shali 
    |http://www.cs.utexas.edu/~wcook/Civet/""")

  override def processOptions(options: List[String], error: String => Unit) = {
    for(option <- options) {
      if(option.startsWith("open")) {
        closed = false
      } else if(option.startsWith("closed")) {
        closed = true
      } else {
        error(s"Option not understood: ${option}")
      }
    }
  }
  override val optionsHelp: Option[String] = 
    Some("""| -P:mina:open            Compiler considers the world as open
            | -P:mina:close           Compiler considers the world as closed
            """.stripMargin)
}


