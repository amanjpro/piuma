import ch.usi.inf.l3.mina._

class Test2(val b: Int, val c: Int) {
  def this() = {
    this(0, 0)
  }
  
  
}

class A {
  def b() = {
    new Test2(CT(2), 3)
  }
}

object Test2{
  def main(args: Array[String]): Unit = {
    println(new Test2(CT(2), 3).b)
  }
}