/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.piuma.util


import ch.usi.inf.l3.piuma.plugin.PiumaPlugin


trait TreesUtil { self: PiumaPlugin =>
  import self._
  import self.global._

  /**
    * A utility method to check whether a name is a unary 
    *
    * @param name the name to be checked
    *
    * @return true if name is unary, and false otehrwise
    */
  def isUop(name: TermName): Boolean = {
    name match {
      case nme.UNARY_~ | nme.UNARY_+ | nme.UNARY_- | nme.UNARY_! => true
      case _ => false
    }
  }

  /**
    * A utility method to check whether a name is a binary 
    *
    * @param name the name to be checked
    *
    * @return true if name is binary, and false otehrwise
    */
  def isBop(name: TermName): Boolean = {
    name match {
      case nme.OR | nme.XOR | nme.AND | nme.EQ | nme.NE | nme.ADD |
        nme.SUB | nme.MUL | nme.DIV | nme.MOD | nme.LSL | nme.LSR |
        nme.ASR | nme.LT | nme.LE | nme.GE | nme.GT | nme.ZOR |
        nme.ZAND | nme.MINUS | nme.PLUS => true
      case _ => false
    }
  }

  /**
    * A utility method to check whether an application is for 
    * Any's constructor
    *
    * @param a the application to be checked
    *
    * @return true if name is an application of Any's constructor,
    * and false otehrwise
    */
  def isAnyConstructor(a: Apply): Boolean = {
    a.symbol.fullName == "java.lang.Object.<init>" ||
      a.symbol.fullName == "scala.lang.Any.<init>"
  }

  /**
    * A utility method to check whether a tree is unary
    *
    * @param select the tree to be checked
    *
    * @return true if the tree is unary, and false otehrwise
    */
  def isUnary(select: Select): Boolean = {
    if (!hasSymbol(select)) {
      false
    } else {
      val rcvr = select.symbol.owner.tpe
      val c = isAnyVal(rcvr)
      val methodName = select.symbol.name.toTermName
      if (c && isUop(methodName)) {
        true
      } else false
    }
  }

  /**
    * A utility method to check whether a tree is binary
    *
    * @param apply the tree to be checked
    *
    * @return true if the tree is binary, and false otehrwise
    */
  def isBinary(apply: Apply): Boolean = {
    if (!hasSymbol(apply)) {
      false
    } else {
      apply.args match {
        case x :: Nil if isAnyVal(x.tpe) =>
          val fun = apply.fun
          val rcvr = fun.symbol.owner.tpe
          val c = isAnyVal(rcvr)
          val method = fun.symbol
          val methodName = method.name.toTermName
          c && isBop(methodName)
        case _ =>
          false
      }
    }
  }


  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  def doUop(l: Literal, name: Name): Literal = {
    doUop(toVal(l), name)
  }

  /**
    * A utility method to perform a binary operation on two literals
    *
    * @param l1 the first literay to apply the operaton on
    * @param l2 the second literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the binary operation
    */
  def doBop(l1: Literal, l2: Literal, name: Name): Literal = {
    doBop(toVal(l1), toVal(l2), name)
  }

  /**
    * A utility method to extract the value of a literal
    *
    * @param lit the literal to be extracted
    *
    * @return the extracted value of a literal
    */
  private def toVal(lit: Literal): Any = {
    val v = lit.value
    if (lit.tpe <:< definitions.BooleanClass.tpe) v.booleanValue
    else if (lit.tpe <:< definitions.ByteClass.tpe) v.byteValue
    else if (lit.tpe <:< definitions.ShortClass.tpe) v.shortValue
    else if (lit.tpe <:< definitions.IntClass.tpe) v.intValue
    else if (lit.tpe <:< definitions.LongClass.tpe) v.longValue
    else if (lit.tpe <:< definitions.FloatClass.tpe) v.floatValue
    else if (lit.tpe <:< definitions.DoubleClass.tpe) v.doubleValue
    else if (lit.tpe <:< definitions.CharClass.tpe) v.charValue
    else if (lit.tpe <:< definitions.StringClass.tpe) v.stringValue
    else fail(s"${lit.tpe} is not a builtin value class")
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(v: Any, name: Name): Literal = {
    v match {
      case x: Boolean => doUop(x, name)
      case x: Byte => doUop(x, name)
      case x: Short => doUop(x, name)
      case x: Int => doUop(x, name)
      case x: Long => doUop(x, name)
      case x: Float => doUop(x, name)
      case x: Double => doUop(x, name)
      case x: Char => doUop(x, name)
      case _ =>
        fail(s"${name} is not a binary operation of " +
          "${fst.getClass} and ${snd.getClass}")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Boolean, snd: Boolean, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case nme.ZOR => Literal(Constant(fst || snd))
      case nme.ZAND => Literal(Constant(fst && snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Boolean, name: Name): Literal = {
    name match {
      case nme.UNARY_! => Literal(Constant(x.unary_!))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Byte, name: Name): Literal = {
    name match {
      case nme.UNARY_~ => Literal(Constant(x.unary_~))
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Short, name: Name): Literal = {
    name match {
      case nme.UNARY_~ => Literal(Constant(x.unary_~))
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Int, name: Name): Literal = {
    name match {
      case nme.UNARY_~ => Literal(Constant(x.unary_~))
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Long, name: Name): Literal = {
    name match {
      case nme.UNARY_~ => Literal(Constant(x.unary_~))
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Float, name: Name): Literal = {
    name match {
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Double, name: Name): Literal = {
    name match {
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doUop(x: Char, name: Name): Literal = {
    name match {
      case nme.UNARY_~ => Literal(Constant(x.unary_~))
      case nme.UNARY_+ => Literal(Constant(x.unary_+))
      case nme.UNARY_- => Literal(Constant(x.unary_-))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: String, snd: String, name: Name): Literal = {
    name match {
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Float, snd: Float, name: Name): Literal = {
    name match {
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Double, snd: Double, name: Name): Literal = {
    name match {
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Byte, snd: Byte, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LSL => Literal(Constant(fst << snd))
      case nme.LSR => Literal(Constant(fst >>> snd))
      case nme.ASR => Literal(Constant(fst >> snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Short, snd: Short, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LSL => Literal(Constant(fst << snd))
      case nme.LSR => Literal(Constant(fst >>> snd))
      case nme.ASR => Literal(Constant(fst >> snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }

  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Long, snd: Long, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LSL => Literal(Constant(fst << snd))
      case nme.LSR => Literal(Constant(fst >>> snd))
      case nme.ASR => Literal(Constant(fst >> snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Int, snd: Int, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LSL => Literal(Constant(fst << snd))
      case nme.LSR => Literal(Constant(fst >>> snd))
      case nme.ASR => Literal(Constant(fst >> snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Any, snd: Any, name: Name): Literal = {
    (fst, snd) match {
      case (x: String, y) => doBop(x, y.toString, name)
      case (y, x: String) => doBop(x, y.toString, name)
      case (y, x: Double) => doBop(x, y.asInstanceOf[Double], name)
      case (x: Double, y) => doBop(x, y.asInstanceOf[Double], name)
      case (y, x: Float) => doBop(x, y.asInstanceOf[Float], name)
      case (x: Float, y) => doBop(x, y.asInstanceOf[Float], name)
      case (y, x: Long) => doBop(x, y.asInstanceOf[Long], name)
      case (x: Long, y) => doBop(x, y.asInstanceOf[Long], name)
      case (y, x: Int) => doBop(x, y.asInstanceOf[Int], name)
      case (x: Int, y) => doBop(x, y.asInstanceOf[Int], name)
      case (y, x: Short) => doBop(x, y.asInstanceOf[Short], name)
      case (x: Short, y) => doBop(x, y.asInstanceOf[Short], name)
      case (y, x: Byte) => doBop(x, y.asInstanceOf[Byte], name)
      case (x: Byte, y) => doBop(x, y.asInstanceOf[Byte], name)
      case (y, x: Boolean) => doBop(x, y.asInstanceOf[Boolean], name)
      case (x: Boolean, y) => doBop(x, y.asInstanceOf[Boolean], name)
      case (y, x: Char) => doBop(x, y.asInstanceOf[Char], name)
      case (x: Char, y) => doBop(x, y.asInstanceOf[Char], name)
      case (_, _) =>
        fail(s"${name} is not a binary operation of " +
          "${fst.getClass} and ${snd.getClass}")
    }
  }
  /**
    * A utility method to perform a unary operation on a literal
    *
    * @param l the literay to apply the operaton on
    * @param name the name of the operation
    *
    * @return the result of the unary operation
    */
  private def doBop(fst: Char, snd: Char, name: Name): Literal = {
    name match {
      case nme.OR => Literal(Constant(fst | snd))
      case nme.XOR => Literal(Constant(fst ^ snd))
      case nme.AND => Literal(Constant(fst & snd))
      case nme.EQ => Literal(Constant(fst == snd))
      case nme.NE => Literal(Constant(fst != snd))
      case nme.ADD | nme.PLUS => Literal(Constant(fst + snd))
      case nme.SUB | nme.MINUS => Literal(Constant(fst - snd))
      case nme.MUL => Literal(Constant(fst * snd))
      case nme.DIV => Literal(Constant(fst / snd))
      case nme.MOD => Literal(Constant(fst % snd))
      case nme.LSL => Literal(Constant(fst << snd))
      case nme.LSR => Literal(Constant(fst >>> snd))
      case nme.ASR => Literal(Constant(fst >> snd))
      case nme.LT => Literal(Constant(fst < snd))
      case nme.LE => Literal(Constant(fst <= snd))
      case nme.GE => Literal(Constant(fst > snd))
      case nme.GT => Literal(Constant(fst >= snd))
      case _ => fail(s"${name} is not a binary operation")
    }
  }


  private def fail(msg: String): Nothing = {
    throw new Error(msg)
  }


}

