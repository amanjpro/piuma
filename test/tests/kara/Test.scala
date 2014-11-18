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

  def mthd[R](@incremental n: Option[R]): Option[R] = {
    val b = n
    if(b == None) 
      return b
    else {
      val r = n
      Some(b.get)
    }

  }

  def id[T](q: T): T = q

  def b(@incremental q: B): Unit = {
    q match {
      case s @ A(k, Some(r)) => 
        s
        println(k)
        println(r)
      case d @ A(s, Some(r: C)) =>
        s
        r
      case d @ A(s, k) =>
        println(3)
        val b = 3
        s
        List(1, 2, 3).foreach((x) => print(k))
        b
      case d: C =>
        println(d)
        println(d)
      case d => 
        d
        println("")
        println("r")
    }
    println("Hello")
  }
  // def b: String = b_
  // def b_=(t: String): Unit = b_ = t
}


trait B

case class A(b: Int, c: Option[C] = None) extends B
case object C extends B
