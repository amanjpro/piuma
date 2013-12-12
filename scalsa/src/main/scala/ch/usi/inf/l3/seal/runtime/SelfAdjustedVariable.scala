package ch.usi.inf.l3.seal.runtime



/**
 * This class is a representation of a Self-Adjusted data, every assignment, 
 * setters and getters of the variable that the instance of this class represents
 * is done through the (read and write) methods in this class. 
 */
class SelfAdjustedVariable[T](private var f: T) {
  /**
   * Self-adjusted writes
   */
  def write(v: T): Unit = f = v
  
  /**
   * Self-adjusted reads
   */
  def read: T = f
}

object SelfAdjustedVariable {
  def apply[T](v: T) = {
    new SelfAdjustedVariable(v)
  }
}
