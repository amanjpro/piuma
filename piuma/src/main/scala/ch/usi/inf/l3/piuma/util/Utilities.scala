/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
package ch.usi.inf.l3.piuma.util

private[piuma] object Utilities { 
 
  
  // Standard scala compiler phases
  val PHASE_PARSER = "parser" // 1
  val PHASE_NAMER = "namer"// 2
  val PHASE_PACKAGEOBJECTS = "packageobjects" // 3
  val PHASE_TYPER = "typer" // 4
  val PHASE_PATMAT = "patmat" // 5
  val PHASE_SUPERACCESSORS = "superaccessors" // 6
  val PHASE_EXTMETHODS = "extmethods" // 7
  val PHASE_PICKLER = "pickler" // 8
  val PHASE_REFCHECKS = "refchecks" // 9
  val PHASE_SELECTIVEANF = "selectiveanf" // 10
  val PHASE_SELECTIVECPS = "selectivecps" // 11
  val PHASE_UNCURRY = "uncurry" // 12
  val PHASE_TAILCALLS = "tailcalls" // 13
  val PHASE_SPECIALIZE = "specialize" // 14
  val PHASE_EXPLICITOUTER = "explicitouter" // 15
  val PHASE_ERASURE = "erasure" // 16
  val PHASE_POSTERASURE = "posterasure" // 17
  val PHASE_LAZYVALS = "lazyvals" // 18
  val PHASE_LAMBDALIFT = "lambdalift" // 19
  val PHASE_CONSTRUCTORS = "constructors" // 20
  val PHASE_FLATTEN = "flatten" // 21
  val PHASE_MIXIN = "mixin" // 22
  val PHASE_CLEANUP = "cleanup" // 23
  val PHASE_ICODE = "icode" // 24
  val PHASE_INLINER = "inliner" // 25
  val PHASE_INLINEEXCEPTIONHANDLERS = "inlineExceptionHandlers" // 26
  val PHASE_CLOSELIM = "closelim" // 27
  val PHASE_DCE = "dce" // 28
  val PHASE_JVM = "jvm" // 29
  val PHASE_TERMINAL = "terminal" // 30 
}
