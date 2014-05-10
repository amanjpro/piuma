package ch.usi.inf.l3.lombrello.dsl.source

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */
import java.io.File
import java.io.IOException
import scala.io.Source


class SourceFile(file: File) {

  def this(name: String) = {
    this(new File(name))
  }

  val canonicalName = file.getCanonicalPath
  val name = file.getName

  private def readLines() = try {
    name match {
      case "" => Array("")
      case _ =>
        val lns = Source.fromFile(file).getLines
        lns.toArray
    }
  } catch {
    case e: IOException => 
      println(s"Cannot read from ${name}")
      println(e.getMessage)
      Array("")
  }

  private val lines = readLines


  val content: List[Char] = lines.mkString("\n").toList



  def line(n: Int) = {
    name match {
      case "" => ""
      case _ => 
        try {
          lines(n - 1)
        } catch {
          case ex: ArrayIndexOutOfBoundsException =>
            println(ex.getMessage)
            ""
        }
    }
  }
}
