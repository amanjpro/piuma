package ch.usi.inf.l3.kara.runtime



/**
 * This class is a representation of a Self-Adjusted data, every assignment, 
 * setters and getters of the variable that the instance of this class represents
 * is done through the (read and write) methods in this class. 
 */
class KaraVariable[T] private (private var f: T) {
  /**
   * Self-adjusted writes
   */
  def write(v: T): Unit = f = v
  
  /**
   * Self-adjusted reads
   */
  def read: T = f
  
}

object KaraVariable {
  def apply[T](v: T) = {
    new KaraVariable(v)
  }
}
