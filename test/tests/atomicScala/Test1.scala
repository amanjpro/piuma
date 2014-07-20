import ch.usi.inf.l3.ascala._
import ch.usi.inf.l3.as.plugin._

object Test1 {
  
}
class MyList(val b: Int, val lck: OrderedLock) {
  @atomic('a) var list_f1 = "f1 list"

  def doSomething(@unitfor('b) n:Node, a: Int) = {
      val listTree = List(OrderedLock(), OrderedLock(), OrderedLock())
      val goodTree = listTree.filter(x => x!= null)
      val b = 10 + a
      b + a
      val mList = new MyList(10, OrderedLock())
      @alias('b, 'a) val mNode = new Node(15)
  }
}

class MyOrderedList(b:Int, lck2: OrderedLock) extends MyList(b, lck2){
  def this(s1: String, b: Int, lck3: OrderedLock) = {this(b, lck3); println("test") }
}

class Node(val value: Int) {
  @atomic('b) var link = "whatever"
}


