package ch.usi.inf.l3.lombrello.dsl



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */


import scala.annotation.tailrec
import parser._
import typechecker._
import reporter._
import source._
import codegen._
import symbols._



class Compiler extends Trees
  with Symbols
  with Types
  with Parsers 
  with Typers
  with Reporters 
  with CodeGenerators
  with Finalizers {


  val SUCCESS = 0
  val FAIL = 1


  val tokens = new Tokens {}
  val reporter = new Report

  lazy val lexer = new Lexer
  lazy val normalizer = new Normalizer
  lazy val parser = new Parser
  lazy val typer = new Typer
  lazy val codegen = new CodeGenerator
  lazy val finalizer = new Finalizer


  // FIXME:
  // Only these two fields are mutable in this compiler
  // If you could easily remove it, then do
  var currentPhase: Phase = lexer

  lazy val phases: List[Phase] = orderPhases(List(
    normalizer,
    parser,
    typer,
    codegen,
    finalizer
  ))

  // TODO: make this tailrec
  private def findNext(phases: List[Phase], phaseName: String): 
      (Option[Phase], List[Phase]) = {
    phases match {
      case Nil => (None, Nil)
      case x :: xs if x.runsAfter != None && phaseName == x.runsAfter.get =>
        (Some(x), xs)
      case x :: xs =>
        val (r, rest) = findNext(xs, phaseName)
        (r, x :: rest)
    }
  }

  // TODO: make this tailrec
  def orderPhases(phases: List[Phase]): List[Phase] = {
    def order(ph: Phase, phaseList: List[Phase]): List[Phase] = {
      phaseList match {
        case x :: Nil => x :: Nil
        case xs =>
          val (r, rest) = findNext(xs, ph.name)
          r match {
            case Some(result) =>
              result :: order(result, rest)
            case None =>
              throw new Error("There is no phase that runs after phase: " +
                ph.name)
          }
      }
    }

    lexer :: order(lexer, phases)
  }


  private def compile1(files: List[SourceFile]): Int = {
    val stime = System.currentTimeMillis
    new Runner(files, stime).run()
  }

  def compile(files: List[String]): Int = {
    compile1(files.map(new SourceFile(_)))
  }

  private class Runner(files: List[SourceFile], stime: Long) {

    def run(): Int = {
      run(phases, files)  
    }


    // TODO: Make this tailrec
    private def run[T](phases: List[Phase], result: T): Int = {
      phases match {
        case x :: xs =>
          try {
            val rr = result.asInstanceOf[x.InputType]
            currentPhase = x
            val r = x.run(rr)
            run(xs, r)
          } catch {
            case ex : ClassCastException =>
              println(ex.getMessage)
              val ftime = System.currentTimeMillis
              println(s"Incompatible compiler phases: ${x.name}, " +
                s"and ${x.runsAfter.getOrElse("lexer")}")
              println(s"[fail] Total time: ${(ftime - stime) / 1000.0} s")
              FAIL
          }
        case Nil =>
          val ftime = System.currentTimeMillis
          val (s1, s2, r) = reporter.errorCounter match {
            case 0 =>
              ("No error", "success", SUCCESS)
            case 1 =>
              reporter.printErrors
              ("1 error", "fail", FAIL)
            case _ =>
              reporter.printErrors
              (s"${reporter.errorCounter} errors", "fail", FAIL)
          }
          println(s"${s1} found")
          println(s"[${s2}] Total time: ${(ftime - stime) / 1000.0} s")
          r
      }
    }
  }
}






