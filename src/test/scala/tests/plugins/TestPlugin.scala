package tests.plugins

import ch.usi.inf.l3.lombrello.transform.dsl._
import scala.tools.nsc.Global

class TestPlugin(override val global: Global) extends TransformerPlugin(global) {
  
  val name: String = "test"
  override val description: String = """A compiler plugin!""";
  
  val components: List[TransformerPluginComponent] = List(new TestPluginComponent(this))
}


class TestPluginComponent(plgn: TestPlugin) extends TransformerPluginComponent(plgn) {
  
  
  override val runsRightAfter = Some(utilities.PHASE_PATMAT)
  val runsAfter = List[String](utilities.PHASE_PATMAT, utilities.PHASE_TYPER)
  override val runsBefore = List[String](utilities.PHASE_SUPERACCESSORS)
  val phaseName = "test"
  
  
  import global._
  
//  def bb(c: Context)(b: c.Expr[Any]): c.Expr[Any] = {
//    import c.universe._
////    val p = q"val b = 3"
//    null
//  }
//  
  def transform(tree: Tree): Tree = {
    tree match {
      case q"val $name = $v" =>
        val b = q"class A(b: Int)"
        b
//        println(name)
//        tree
      case x => x
    }
  }
}