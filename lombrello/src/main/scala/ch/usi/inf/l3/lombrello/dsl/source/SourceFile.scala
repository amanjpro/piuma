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

  private val lines = try {
    Source.fromFile(file).getLines.toArray
  } catch {
    case e: IOException => Array("")
  }
  val content: List[Char] = lines.mkString("\n").toList

  def line(n: Int) = lines(n - 1)
}
