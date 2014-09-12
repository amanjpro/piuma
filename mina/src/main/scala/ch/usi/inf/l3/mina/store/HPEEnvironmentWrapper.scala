/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.mina.store

import ch.usi.inf.l3.mina.eval._
import ch.usi.inf.l3.mina._

trait HPEEnvironmentWrapper {
  self: HPE =>
  import self.global._

  // ---------------------- Environment ---------------------------------------

  class Environment private (private val location: Map[Symbol, Int],
    private val store: Map[Int, Value],
    private val loc: Int) {

    private def this() {
      this(Map.empty, Map.empty, -1)
    }

    def getValue(s: Symbol): Value = {
      if (s == null || s == NoSymbol)
        throw new HPEError(s"Symbol should not be null or NoSymbol ${s}")
      location.get(s) match {
        case None => Bottom
        case Some(x) => store(x)
      }
    }

    def addValues(valvars: List[(Symbol, Value)]): Environment = {
      var env = this
      var tail = valvars
      while (tail != Nil) {
        val (vr, vl) = tail.head
        env = env.addValue(vr, vl)
        tail = tail.tail
      }
      env
    }
    def updateValue(s: Symbol, value: Value): Environment = {
      if (s == null || s == NoSymbol)
        throw new HPEError(s"Symbol should not be null or NoSymbol ${s}")
      location.get(s) match {
        case None =>
          addValue(s, value)
        case Some(l) =>
          val old = store(l)
          old match {
            case Bottom | _ if (old.tpe == value.tpe) =>
              val s = store + (l -> value)
              new Environment(location, s, l)
            case _ => throw new HPEError(s"Once you make a variable, CT, " +
              "Abstract or Top you may not change it ${v}")
          }

      }
    }
    def addValue(s: Symbol, value: Value): Environment = {
      if (s == null || s == NoSymbol)
        throw new HPEError(s"Symbol should not be null or NoSymbol ${s}")
      val l = loc + 1
      val m = location + (s -> l) //location + (s -> l)
      val st = store + (l -> value)
      new Environment(m, st, l)
    }
    private def makeConsistent(x: Environment, y: Environment): Environment = {
      var r = this
      for ((t, l) <- x.location) {

        val vx = x.getValue(t)
        val vy = y.getValue(t)
        if (vx == vy) r.addValue(t, vx)
        else if (vy == Top || vx == Top) r.addValue(t, Top)
        else r.remove(t)

      }
      r
    }

    def makeConsistent(envs: List[Environment]): Environment = {
      envs match {
        case Nil => Environment.empty
        case x :: Nil => x
        case x :: y :: Nil =>
          if (x.location.size >= y.location.size) makeConsistent(x, y)
          else makeConsistent(y, x)
        case x :: y :: xs =>
          val x1 = makeConsistent(x :: y :: Nil)
          makeConsistent(x1 :: xs)
      }
    }

    /**
     * A very important method for handling function calls on objects accurately.
     *
     * This method makes sure to correctly pass all the parameter to the called
     * method while still preserving the store of the object.
     *
     * @param params a list of tuple of method parameters
     * @param sourse the source environment, to look for variables values from
     * @return a new environment which has all the information of its parameters
     */
    def addBatch(vars: List[Symbol],
      source: Environment): Environment = {
      var tail = vars
      var tempStore = this
      while (tail != Nil) {
        val head = tail.head
        tail = tail.tail
        val l = source.location(head)
        val value = source.store(l)
        val loc = tempStore.loc + 1
        tempStore = new Environment(tempStore.location + (head -> l),
          tempStore.store + (l -> value),
          loc)
      }

      tempStore
    }

    /**
     * Removes a variable from the environment
     *
     * @param s the variable to be removed
     * @return a new environment, which has the bindings of variable v removed
     */
    def remove(s: Symbol): Environment = {
      val location2 = location - (s)
      new Environment(location2, store, loc)
    }

    def remove(vars: List[Symbol]): Environment = {
      var tail = vars
      var tempStore = this
      while (tail != Nil) {
        val head = tail.head
        tail = tail.tail
        tempStore = remove(head)
      }
      tempStore
    }

    private def getPEValue(s: Symbol): Option[Value] = {
      location.get(s) match {
        case Some(loc) => store.get(loc)
        case _ => None
      }
    }

    def isCT(s: Symbol): Boolean = {
      getPEValue(s) match {
        case Some(CTValue(_)) => true
        case _ => false
      }
    }

    def isRT(s: Symbol): Boolean = {
      getPEValue(s) match {
        case Some(Top) => true
        case _ => false
      }
    }

    override def toString: String = location.toString + "\n" + store.toString
  }

  object Environment {
    def empty: Environment = { new Environment }
    def apply(varval: List[(Symbol, Value)]): Environment = {
      newStore(varval)
    }
    def apply(varval: (List[Symbol], List[Value])): Environment = {
      newStore(varval._1 zip varval._2)
    }
    def newStore(varval: List[(Symbol, Value)]): Environment = {
      var env = new Environment
      var tail = varval
      while (tail != Nil) {
        val (x, v) = tail.head
        env = env.addValue(x, v)
        tail = tail.tail
      }
      env
    }
  }

  // ---------------------- Value -----------------------------------------
  sealed trait Value {
    protected val BOTTOM = 0
    protected val CT = 1
    protected val RT = 2
    protected val TOP = 3
    def value: Option[HPEAny];
    val isCT = false
    def tpe: Int;
  }

  case object Bottom extends Value {
    override def value: Option[HPEAny] = None
    def tpe: Int = BOTTOM
  }

  case object Top extends Value {
    override def value: Option[HPEAny] = None
    def tpe: Int = TOP
  }

  case class CTValue(v: HPEAny) extends Value {
    override def value: Option[HPEAny] = Some(v)
    override def toString: String = value.get.toString
    def toTree = v.tree
    override val isCT = true 
    def tpe: Int = CT
  }

  case class AbsValue(v: HPEAny) extends Value {
    override def value: Option[HPEAny] = Some(v)
    def toCTValue = CTValue(v)
    def tpe: Int = RT
  }

  // ---------------------- Simulating Runtime Object  -----------------------

  trait HPEAny {
    val tree: Tree;
    val tpe: Type;
  }
  case class HPEObject(val tree: Tree, val tpe: Type,
    val store: Environment) extends HPEAny {
    override def equals(that: Any): Boolean = {
      that match {
        case HPEObject(_, `tpe`, `store`) => true
        case _ => false
      }
    }
    override def toString: String = tree.toString.replaceAll("[\\[\\]]", "")
    override def hashCode = 71 * 5 + tpe.## + store.##
  }

  case class HPELiteral(override val tree: Literal,
    override val tpe: Type) extends HPEAny {
    override def equals(that: Any): Boolean = {
      that match {
        case HPELiteral(t, `tpe`) => tree.value.value == t.value.value
        case _ => false
      }
    }

    override def toString: String = tree.value.toString
    override def hashCode = 71 * 5 + tree.value.value.## + tpe.##
  }

  case class HPETree(val tree: Tree) extends HPEAny {
    val tpe: Type = tree.tpe
  }
}