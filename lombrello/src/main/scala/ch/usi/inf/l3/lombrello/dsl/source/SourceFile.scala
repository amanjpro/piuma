package ch.usi.inf.l3.lombrello.dsl.source

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */
import java.io.File
import scala.io.Source


class SourceFile(file: File) {

  def this(name: String) = {
    this(new File(name))
  }

  val canonicalName = file.getCanonicalPath
  val name = file.getName

  private val lines = Source.fromFile(file).getLines.toArray
  val content: List[Char] = lines.mkString("\n").toList

  def line(n: Int) = lines(n - 1)
}
