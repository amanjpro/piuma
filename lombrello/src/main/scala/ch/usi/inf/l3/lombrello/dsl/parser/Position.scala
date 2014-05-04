package ch.usi.inf.l3.lombrello.dsl.parser

/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */
import ch.usi.inf.l3.lombrello
import lombrello.dsl.source.SourceFile

final class Position(private val file: SourceFile, val col: Int, val row: Int) {
  override def toString: String = {
    s"${file.canonicalName}: ${row}"
  }

  def copy(file: SourceFile = file, col: Int = col, row: Int = row): Position = {
    Position(file, col, row)
  }
  override def equals(other: Any): Boolean = {
    other match {
      case null => false
      case that: Position =>
        this.file == that.file && this.file == that.col && this.row == that.row
      case _ =>
        false
    }
  }

  override def hashCode: Int = {
    41 * (41 + file.hashCode + col.hashCode + row.hashCode)
  }

  val line = file.line(row)
  val canonicalName = file.canonicalName
  val name = file.name
}

object Position {
  def apply(): Position = {
    new Position(new SourceFile(""), 0, 0)
  }

  def apply(file: SourceFile, col: Int, row: Int): Position = {
    new Position(file, col, row)
  }

  def unapply(pos: Position): Option[(SourceFile, Int, Int)] = {
    Some((pos.file, pos.col, pos.row))
  }

}


