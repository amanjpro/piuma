package ch.usi.inf.l3.lombrello.dsl.codegen



/**
 * @author Amanj Sherwany
 * @date 8 May 2014
 */

import java.io.File
import java.io.IOException
import java.io.PrintWriter
import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.source._
import scala.annotation.tailrec


case class CompiledCode(name: String, content: String) {

  private val sep = File.separatorChar
  def writeToFile(): Int = {
    try {
      val (path, _) = split
      new File(path).mkdirs
      val file = new File(name)
      val writer = new PrintWriter(file)
      writer.write(content)
      writer.close
      0
    } catch {
      case ex: IOException =>
        println(s"Couldn't write to the file: ${name}")
        println(ex.getMessage)
        1
    }
  }

  private def split(): (String, String) = {
    val (path, nme) = name.splitAt(name.lastIndexOf(sep))
    (path.substring(0, path.length), nme)
  }
}
