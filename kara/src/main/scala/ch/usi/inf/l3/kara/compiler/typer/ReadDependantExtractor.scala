package ch.usi.inf.l3.kara.compiler.typer

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.kara.KaraPlugin
import ch.usi.inf.l3.kara.quals.incremental
import ch.usi.inf.l3.kara.runtime.KaraVariable
import ch.usi.inf.l3.lombrello.util.Utilities

class ReadDependantExtractor(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some("kara-typer")
  val runsAfter = List[String]("kara-typer")
  override val runsBefore = List[String](plgn.utilities.PHASE_JVM)
  val phaseName = "kara-mover"
  import plugin.global._
  import plugin._

  val selfModule = rootMirror.requiredModule[KaraVariable.type] //.getModuleByName(newTermName(plgn.selfAdjustedClassName))
  val applySymbol = selfModule.filter(_.name == newTermName("apply"))
  val readSymbol = selfModule.filter(_.name == newTermName("read"))
  val writeSymbol = selfModule.filter(_.name == newTermName("write"))

  definitions.PredefModule
  val incAnno = getAnnotation(plgn.karaAnnotationName)

  def doTransform(cmp: TransformerComponent, tree: Tree): (Tree, Boolean) = {
    tree match {
      case templ: Template =>
        val newBody = templ.body.map(doTransform(cmp, _)._1)
        (templ.copy(body = newBody).setSymbol(templ.symbol), false)
      case mthd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        if (rhs.exists(_.symbol == readSymbol)) {
          // TODO you should extract out from where the read exists to the end 
          // of the method

          // challenges? the extraction should take care of all the variables 
          // that is initialized earlier in this method and is used later after 
          // the read
        }
        (mthd, false)
      case _ =>
        // do something else
        (tree, true)
    }
  }
  def transform(cmp: TransformerComponent, tree: Tree): (Tree, Boolean) = {
    tree match {
      case _ => (tree, false)
      case clazz @ ClassDef(mods, name, tparamss, impl) =>
        val newImpl = doTransform(cmp, impl).asInstanceOf[Template]
        val newClazz = clazz.copy(impl = newImpl).setSymbol(clazz.symbol)
        (cmp.localTyper.typed { newClazz }, false)
      case module @ ModuleDef(mods, name, impl) =>
        val newImpl = doTransform(cmp, impl).asInstanceOf[Template]
        val newModule = module.copy(impl = newImpl).setSymbol(module.symbol)
        (cmp.localTyper.typed { newModule }, false) 
    }
  }

}
