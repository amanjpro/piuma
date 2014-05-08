package ch.usi.inf.l3.lombrello.dsl.codegen



/**
 * @author Amanj Sherwany
 * @date 8 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.source._
import scala.annotation.tailrec


case class CompiledCode(name: String, content: String)
