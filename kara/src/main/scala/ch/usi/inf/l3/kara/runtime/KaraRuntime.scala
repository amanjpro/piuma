
// The following file is generated using a computer program.
// The generator program is: runtimeGenerator

package ch.usi.inf.l3.kara.runtime

/*
 * Copyright (c) <2013-2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
  

/**
 * A dumb class just to make it possible to access the KaraRuntime module
 * in the compiler.
 */
class KaraRuntime {}


/**
 * Why are not we using an identity based HashMap?
 * Because if we do so, then we can change a mutable variable's state without
 * letting the cache notices, that way we get unexpected results.
 */
object KaraRuntime {

  private var map0: Map[String, Any] = Map.empty
  private var map1: Map[(String, List[Any]), Any] = Map.empty
  private var map2: Map[(String, List[Any]), Any] = Map.empty
  private var map3: Map[(String, List[Any]), Any] = Map.empty
  private var map4: Map[(String, List[Any]), Any] = Map.empty
  private var map5: Map[(String, List[Any]), Any] = Map.empty
  private var map6: Map[(String, List[Any]), Any] = Map.empty
  private var map7: Map[(String, List[Any]), Any] = Map.empty
  private var map8: Map[(String, List[Any]), Any] = Map.empty
  private var map9: Map[(String, List[Any]), Any] = Map.empty
  private var map10: Map[(String, List[Any]), Any] = Map.empty
  private var map11: Map[(String, List[Any]), Any] = Map.empty
  private var map12: Map[(String, List[Any]), Any] = Map.empty
  private var map13: Map[(String, List[Any]), Any] = Map.empty
  private var map14: Map[(String, List[Any]), Any] = Map.empty
  private var map15: Map[(String, List[Any]), Any] = Map.empty
  private var map16: Map[(String, List[Any]), Any] = Map.empty
  private var map17: Map[(String, List[Any]), Any] = Map.empty
  private var map18: Map[(String, List[Any]), Any] = Map.empty
  private var map19: Map[(String, List[Any]), Any] = Map.empty
  private var map20: Map[(String, List[Any]), Any] = Map.empty
  private var map21: Map[(String, List[Any]), Any] = Map.empty
  private var map22: Map[(String, List[Any]), Any] = Map.empty



  def runClosure[R](n: String, f: Function0[R]): R = {
    val r = map0.get(n) match {
      case None =>
        val x = f()
        map0 += (n -> x)
        x
      case Some(x) => x
    }
    r.asInstanceOf[R]
  }



  
  
    def runClosure[T0, R](n: String, f: Function1[T0, R], a0: T0): R = {
      val l = List(a0)
      val r = map1.get((n, l)) match {
        case None =>
          val x = f(a0)
          map1 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, R](n: String, f: Function2[T0, T1, R], a0: T0, a1: T1): R = {
      val l = List(a0, a1)
      val r = map2.get((n, l)) match {
        case None =>
          val x = f(a0, a1)
          map2 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, R](n: String, f: Function3[T0, T1, T2, R], a0: T0, a1: T1, a2: T2): R = {
      val l = List(a0, a1, a2)
      val r = map3.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2)
          map3 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, R](n: String, f: Function4[T0, T1, T2, T3, R], a0: T0, a1: T1, a2: T2, a3: T3): R = {
      val l = List(a0, a1, a2, a3)
      val r = map4.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3)
          map4 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, R](n: String, f: Function5[T0, T1, T2, T3, T4, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4): R = {
      val l = List(a0, a1, a2, a3, a4)
      val r = map5.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4)
          map5 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, R](n: String, f: Function6[T0, T1, T2, T3, T4, T5, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5): R = {
      val l = List(a0, a1, a2, a3, a4, a5)
      val r = map6.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5)
          map6 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, R](n: String, f: Function7[T0, T1, T2, T3, T4, T5, T6, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6)
      val r = map7.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6)
          map7 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, R](n: String, f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7)
      val r = map8.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7)
          map8 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, R](n: String, f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8)
      val r = map9.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
          map9 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R](n: String, f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
      val r = map10.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
          map10 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](n: String, f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
      val r = map11.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
          map11 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](n: String, f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
      val r = map12.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
          map12 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](n: String, f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
      val r = map13.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
          map13 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](n: String, f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
      val r = map14.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
          map14 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](n: String, f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
      val r = map15.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
          map15 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](n: String, f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
      val r = map16.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
          map16 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](n: String, f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
      val r = map17.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
          map17 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](n: String, f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
      val r = map18.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
          map18 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](n: String, f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
      val r = map19.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
          map19 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](n: String, f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
      val r = map20.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
          map20 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](n: String, f: Function21[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
      val r = map21.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
          map21 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  
  
  
    def runClosure[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](n: String, f: Function22[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R], a0: T0, a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9, a10: T10, a11: T11, a12: T12, a13: T13, a14: T14, a15: T15, a16: T16, a17: T17, a18: T18, a19: T19, a20: T20, a21: T21): R = {
      val l = List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
      val r = map22.get((n, l)) match {
        case None =>
          val x = f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
          map22 += ((n, l) -> x)
          x
        case Some(x) => x
      }
      r.asInstanceOf[R] 
    }
  
   
  }
