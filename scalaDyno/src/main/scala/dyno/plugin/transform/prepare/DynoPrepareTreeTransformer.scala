package dyno.plugin
package transform
package prepare
import scala.tools.nsc.transform.InfoTransform
import scala.reflect.internal.Flags._
import scala.reflect.internal.util.Position
import collection.mutable.Map
import ch.usi.inf.l3.lombrello.neve.NeveDSL._


@info("-prepare") class DynoPrepareTreeTransformer {
  // this: DynoPreparePhase =>

  // import helper._

  rightAfter("typer")

  plugin Dyno

  val errorList: Map[Position, String] = plgn.errorList

  def transformInfo(sym: Symbol, info: Type): Type = {
    if ((sym.isClass || sym.isTrait || sym.isModule) 
                            && currentRun.compiles(sym))
      for (mbr <- info.decls if mbr.tpe.isErroneous)
        info.decls.unlink(mbr)

    info
  }

  def revertReporter(): Unit = {
    global.reporter match {
      case rep: OurHackedReporter =>
        global.reporter = rep.orig
      case _ =>
    }
  }

  private def treeToErrString(tree:Tree):String = {
    val errors = ErrorCollector.collect(tree)
    if (errors.isEmpty)
      null
    else
      "Deferred compile-time error(s):\n\n" + errors.map{ 
        case (pos, str) => 
          Position.formatMessage(pos, str, 
              false)}.mkString("\n") + "\nStacktrace:"
  }

  private def treeToException(tree:Tree):Tree = {
    val str = treeToErrString(tree)
    if (str != null) {
      val tree0 = gen.mkSysErrorCall(str) //factory for creating trees
      localTyper.typed(tree0) 
      //localTyper -> keep track of owner and scope to correctly type new nodes
    } else {
      localTyper.typed(Literal(Constant(())))
    }
  }

  def transform(tree: Tree): Tree = { //[T <: Tree]
    //println("prep: " + tree.getClass + " tpe: "+ tree.tpe + " tree: " +tree)
    //transform1 => do the matching
    tree match {
      case Match(selector, cases) if (matchIsErroneous(selector, cases)) =>
        treeToException(tree)
      case x if (x.isErroneous) =>
        treeToException(tree)
      case _:DefTree if (tree.symbol.isErroneous) =>
         treeToException(tree)
      case x =>
        super.transform(x)
    }
  }

  private def matchIsErroneous(selector:Tree, cases:List[CaseDef]) = {
    selector.isErroneous || 
      cases.exists(c => c.pat.isErroneous || c.guard.isErroneous)
  }


  object ErrorCollector extends Traverser {
    var buffer: List[(Position, String)] = Nil

    def add (item:(Position, String)) {
      buffer = item :: buffer
    }
    override def traverse(tree: Tree) = tree match {
      case _ =>
        errorList.get(tree.pos) match {
          case None =>
          case Some(err) => buffer ::= tree.pos -> err
        }// if there's any error for tree.pos, store it in the buffer
        super.traverse(tree)
    }

    def collect(tree: Tree): List[(Position, String)] = {
      buffer = Nil
      traverse(tree)
      buffer
    }
  }

  var dynoPreparePhase : StdPhase = _
  def afterPrepare[T](op: => T): T = global.exitingPhase(dynoPreparePhase)(op)
  def beforePrepare[T](op: => T): T = global.enteringPhase(dynoPreparePhase)(op)

}
