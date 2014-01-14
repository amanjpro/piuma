/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
import ch.usi.inf.l3.kara.quals._

class Test {
  @incremental final var b = "Hello"
  def m[T <: AnyRef](t: T) = {
    @incremental var local = "Hell"
    val c = local + " HEEE"
    local = local + " kkk"
    local + t.toString + b
  }
  // def b: String = b_
  // def b_=(t: String): Unit = b_ = t
}


// class Test {
//   private b_: KaraVariable[String] = KaraVariable.apply("Hello")
//
//   def m(): String = {
//     "hello"
//   }
//
//   private def b(c: String) = {}
// }


//
// class Test {
//   @incremental var b_ :KaraVariable[String] = KaraVariable.apply("Hello")
//   def m() = {
//     // val c = b
//     // c
//     "hello"
//   }
//
//   def b: String = Test.this.b_.read()
//   def b_=(t: String): Unit = b_.write(t)  
// }
