
package ch.usi.inf.l3.kara

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.lombrello.util._
import ch.usi.inf.l3.kara.compiler.typer.KaraTyper
import ch.usi.inf.l3.kara.compiler.typer.ReadDependantExtractor

class KaraPlugin(override val global: TGlobal) extends TransformerPlugin(global) {

  val rightBeforeFinder = utilities.PHASE_TYPER
  val name: String = "scalsa"
  override val description: String = """A compiler plugin to add self-adjusted variables to Scala!""";
//                               ch.usi.inf.l3.kara.runtime.KaraVariable
  val karaClassName = "ch.usi.inf.l3.kara.runtime.KaraVariable"
  val karaAnnotationName = "ch.usi.inf.l3.kara.quals.incremental"
  val pluginComponents: List[TransformerPluginComponent] = 
    			List(new KaraTyper(this), new ReadDependantExtractor(this))
}
