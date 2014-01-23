/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
import ch.usi.inf.l3.kara.quals._

class Test {
  def m[T <: AnyRef](t: T, d: String) = {
    @incremental var local = "Hell"
    var b = local
    var c = local + " HEEE"
    local = local + " kkk"
    local = "3"
    c = local
    c = "hh"
    local + c + t.toString + d + b
  }
  // def b: String = b_
  // def b_=(t: String): Unit = b_ = t
}


