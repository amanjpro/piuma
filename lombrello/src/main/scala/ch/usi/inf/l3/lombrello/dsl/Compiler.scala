package ch.usi.inf.l3.lombrello.dsl



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import java.io.File
import parser._
import reporter._



class Compiler extends Trees
  with Parsers 
  with Reporters {

  val SUCCESS = 0
  val FAIL = 1


  val tokens = new Tokens {}
  val reporter = new Report

  lazy val lexer = new Lexer
  lazy val normalizer = new Normalizer
  lazy val parser = new Parser


  // FIXME:
  // Only these two fields are mutable in this compiler
  // If you could easily remove it, then do
  var currentPhase: Phase = lexer
  var errorCounter = 0

  lazy val phases: List[Phase] = orderPhases(List(
    normalizer,
    parser
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


  private def compile1(files: List[File]): Int = {
    val stime = System.currentTimeMillis
    new Runner(files, stime).run()
  }

  def compile(files: List[String]): Int = {
    compile1(files.map(new File(_)))
  }

  private class Runner(files: List[File], stime: Long) {

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
              val ftime = System.currentTimeMillis
              println(s"Incompatible compiler phases: ${x.name}, " +
                s"and ${x.runsAfter.getOrElse("lexer")}")
              println(s"[fail] Total time: ${(ftime - stime) / 1000.0} s")
              FAIL
          }
        case Nil =>
          val ftime = System.currentTimeMillis
          val (s1, s2, r) = errorCounter match {
            case 0 =>
              ("No error", "success", SUCCESS)
            case 1 =>
              ("1 error", "fail", FAIL)
            case _ =>
              (s"${errorCounter} errors", "fail", FAIL)
          }
          println(s"${s1} found")
          println(s"[${s2}] Total time: ${(ftime - stime) / 1000.0} s")
          r
      }
    }
  }
}






