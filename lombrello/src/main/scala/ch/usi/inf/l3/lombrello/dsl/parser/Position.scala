package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */
import java.io.File

case class Position(file: File, col: Int, row: Int) {
  override def toString: String = {
    s"${file.getCanonicalPath}: ${row}"
  }
}
