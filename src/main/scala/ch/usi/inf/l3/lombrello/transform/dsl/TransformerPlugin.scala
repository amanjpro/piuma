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

abstract class TransformerPlugin(val global: Global) 
	extends Plugin {
  
  import global._
  
  val name: String;
  
  val description: String = """A compiler plugin!""";
  
  val components: List[TransformerPluginComponent];
  
}