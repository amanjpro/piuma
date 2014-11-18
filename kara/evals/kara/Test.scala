import ch.usi.inf.l3.kara.quals.incremental
import ch.usi.inf.l3.kara.runtime._ 

trait Tree {
  def weight(): Double
  def getRightNode(n: Int): Option[Node]
  def weight1(): Double
} 


class Node(var v: Int, var l: Tree, var r: Tree) extends Tree {

  def weight(): Double = {
    @incremental val il = l
    val sumL = il.weight() 
    @incremental val thisV = v 
    val sumV = thisV 
    @incremental val ir = r
    val sumR = ir.weight() 
    Math.pow(Math.PI * (sumL), 3) / (Math.PI * sumV) + Math.PI * (sumR)
  }
 
  def getRightNode(n: Int): Option[Node] = {
    if(n == 0) None
    if(n == 1 && r.isInstanceOf[Node]) Some(r.asInstanceOf[Node]) 
    else r.getRightNode(n-1)
  } 
  def weight1(): Double = {
    Math.pow(Math.PI * (l.weight1), 3) / (Math.PI * v) + Math.PI * (r.weight1)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case n: Node => n.v == v && n.l == l && n.r == r
      case _ => false
    }
  }

  override def hashCode: Int = v.## + l.## + r.##
  override def toString: String = {
    s"{${v}, ${l}, ${r}}"
  }
}
object Node {
  def apply(v: Int, l: Tree, r: Tree): Node = new Node(v, l, r)
  
}

class Leaf(val v: Int) extends Tree {

  def getRightNode(n: Int) = None
  def weight(): Double = Math.PI / Math.pow(Math.PI * v, 3)
  def weight1(): Double = Math.PI / Math.pow(Math.PI * v, 3)
  
  override def equals(other: Any): Boolean = {
    other match {
      case n: Leaf => n.v == v
      case _ => false
    }
  }

  override def hashCode: Int = v.## 
  override def toString: String = {
    v.toString
  }
}
object Leaf {
  def apply(v: Int): Leaf = new Leaf(v)
}


object Test { 

  // Manually implemented Incremental Fibonacci method
  def fibonacci3(n: BigInt): BigInt = {
    if(n == 1 || n == 0) n
    else {
      val in = KaraVariable(n)
      return KaraRuntime.runClosure[BigInt, KaraVariable[BigInt], BigInt]("fibonacci31m",
        fibonacci31m, in.read - 1, in)
    }
  }

  def fibonacci31m(f: BigInt, in: KaraVariable[BigInt]): BigInt = {
    val res1 = fibonacci3(f)
    return KaraRuntime.runClosure[BigInt, BigInt, BigInt]("fibonacci311m",
        fibonacci311m, res1, in.read - 2)
  }

  def fibonacci311m(f: BigInt, s: BigInt): BigInt = {
    val res2 = fibonacci3(s)
    f + res2 
     
  }

  def fibonacci9(@incremental n: BigInt): BigInt = {
    if(n == 1 || n == 0) n
    else {
      val next = n - 1
      val res1 = fibonacci9(next)
      val secondNext = n - 2
      val res2 = fibonacci9(secondNext)
      res1 + res2
    }
  }

  // Fibonacci method with the help of @incremental
  def fibonacci1(@incremental n: Int): BigInt = {
    n match {
      case 1 | 0 => n
      case _ => 
        val next = n - 1
        val res1 = fibonacci1(next)
        val secondNext = n - 2
        val res2 = fibonacci1(secondNext)
        res1 + res2
    }
  }

  // Naiive implmentation of Fibonacci method
  def fibonacci2(n: Int): BigInt = {
    if(n == 1 || n == 0) n
    else 
      fibonacci2(n-1) + fibonacci2(n-2)
  }


  // Manually optimized Fibonacci method
  def fibonacci4(n:Int):BigInt = {
   def fibs(n:Int):(BigInt,BigInt) = 
     if (n == 1 || n == 0) (1,0) else {
       val (a,b) = fibs(n/2)
       val p = (2*b+a)*a
       val q = a*a + b*b
       if(n % 2 == 0) (p,q) else (p+q,p)
     }
   n match {
     case 0 | 1 => n
     case _ => fibs(n)._1
   }
  }








  // Left-overs (I left them since it is also a test for correctness)
  def without(tree: Node): (Long, List[Double]) = {
    val t1 = System.currentTimeMillis
    val b = tree.weight1
    val rest = for(i <- 1 to 100) yield {
      var b = tree.getRightNode(i) 
      b match {
        case x: Some[Node] =>  
          
          x.get.r = Node(i, Leaf(i+1), Leaf(10+1))
        case _ => ()
      } 
      tree.weight1
    }
    val t2 = System.currentTimeMillis
    (t2-t1, b :: rest.toList)
  } 
  


  def karawith(tree: Node): (Long, List[Double]) = {
    val t1 = System.currentTimeMillis
    val b = tree.weight
    val rest = for(i <- 1 to 100) yield {
      var b = tree.getRightNode(i) 
      b match {
        case x: Some[Node] =>  
          x.get.r = Node(i, Leaf(i+1), Leaf(10+1))
        case _ => ()
      } 
      tree.weight
    }
    val t2 = System.currentTimeMillis
    (t2-t1, b :: rest.toList)
  } 
  def main(args: Array[String]): Unit = {
    val arg2 = args(0).toInt


    val t1 = System.currentTimeMillis
    val b1 = fibonacci1(arg2)
    val t11 = System.currentTimeMillis

    val t2 = System.currentTimeMillis
    val b2 = fibonacci2(arg2)
    val t22 = System.currentTimeMillis


    val t4 = System.currentTimeMillis
    val b4 = fibonacci4(arg2)
    val t44 = System.currentTimeMillis

    println("Fib Case:    " + (t11 - t1) + "  " + b1)
    println("Fib Naive:   " + (t22 - t2) + "  " + b2)
    println("Fib Fast:   " + (t44 - t4) + "  " + b4)
  //   val tree2 = Node(5, Node(6, Leaf(7), Leaf(8)), Node(1, Leaf(10), Leaf(9)))
  //   val (diff2, res2) = karawith(tree2)

  //   val tree1 = Node(5, Node(6, Leaf(7), Leaf(8)), Node(1, Leaf(10), Leaf(9)))
  //   val (diff1, res1) = without(tree1)
  //   println("without: time diff: " + diff1)
  //   println("with: time diff: " + diff2)
  //   println(res1 == res2)
  }
}
