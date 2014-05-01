package ch.usi.inf.l3.lombrello.dsl



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import java.io.File
import parser._



class Compiler extends Trees
  with Parsers {

  val SUCCESS = 0
  val FAIL = 1

  lazy val lexer = new Lexer
  lazy val parser = new Parser


  // FIXME:
  // Only these two fields are mutable in this compiler
  // If you could easily remove it, then do
  var currentPhase: Phase = lexer
  var errorCounter = 0

  lazy val phases: List[Phase] = orderPhases(List{
    parser
  })

  private def findNext(phases: List[Phase], phaseName: String): 
      (Option[Phase], List[Phase]) = {
    phases match {
      case Nil => (None, Nil)
      case x :: xs if(phaseName == x.runsAfter) =>
        (Some(x), xs)
      case x :: xs =>
        val (r, rest) = findNext(xs, phaseName)
        (r, x :: rest)
    }
  }
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

    def run[T](phases: List[Phase], result: T)
        (implicit m: scala.reflect.Manifest[T]): Int = {
      phases match {
        case x :: xs =>
          m match {
            case rr: x.ReifiedInput =>
              currentPhase = x
              val r = x.run(result.asInstanceOf[x.InputType])
              run(xs, rr)
            case _ =>
              val ftime = System.currentTimeMillis
              println(s"Incompatible compiler phases: ${x.name}, " +
                s"and ${x.runsAfter.getOrElse("lexer")}")
              println(s"[fail] Total time: ${(ftime - stime) / 1000.0} s")
              FAIL
          }
        case Nil if errorCounter == 0 =>
          val ftime = System.currentTimeMillis
          println(s"[success] Total time: ${(ftime - stime) / 1000.0} s")
          SUCCESS
        case Nil =>
          val ftime = System.currentTimeMillis
          errorCounter match {
            case 1 =>
              println("1 error found")
            case _ =>
              println("${errorCounter} errors found")
          }
          println(s"[fail] Total time: ${(ftime - stime) / 1000.0} s")
          FAIL
      }
    }
  }
}






