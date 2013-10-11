package ch.usi.inf.l3.lombrello.transform.dsl


import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.ast.parser.TreeBuilder
import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import ch.usi.inf.l3.lombrello.transform.api.SymbolTreeMap
import scala.tools.nsc.doc.model.TreeFactory

abstract class TransformerPlugin(val global: Global) 
	extends Plugin {
  
  import global._
  
  
  val name: String;
  
  val description: String = """A compiler plugin!""";
  
  val components: List[TransformerPluginComponent];
  
  val treeBank = new {
    val global: TransformerPlugin.this.global.type = TransformerPlugin.this.global
  } with SymbolTreeMap {}
}