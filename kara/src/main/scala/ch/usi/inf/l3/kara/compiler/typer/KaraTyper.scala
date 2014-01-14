package ch.usi.inf.l3.kara.compiler.typer

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.kara.KaraPlugin
import ch.usi.inf.l3.kara.quals._
import ch.usi.inf.l3.kara.runtime.KaraVariable

class KaraTyper(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some(plgn.utilities.PHASE_SUPERACCESSORS)
  val runsAfter = List[String](plgn.utilities.PHASE_FLATTEN, plgn.utilities.PHASE_PATMAT, plgn.utilities.PHASE_TYPER)
  override val runsBefore = List[String]("kara-mover")
  val phaseName = "kara-typer"
  import plugin.global._
  import plugin._

  val karaClass = rootMirror.getClassByName(newTypeName(s"${plgn.karaClassName}"))
  val karaModule = karaClass.companionModule
  val applyName = newTermName("apply")
  val readName = newTermName("read")
  val writeName = newTermName("write")

  val karaAnnotation = getAnnotation(plgn.karaAnnotationName)

  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    tree match {
      case v: ValDef if hasAnnotation(v, karaAnnotation) =>
                assert(isVar(v) && (isFinal(v) || v.symbol.isLocal),
                  s"""|Only variables with final qualifier is allowed to be @incremental,
                        |therefore you cannot self adjust: ${v}""".stripMargin)
        val karaType = TypeRef(NoPrefix, karaClass, List(v.symbol.info))
        val applyTree = Apply(TypeApply(Select(Ident(karaModule), applyName), List(v.tpt)), List(v.rhs))
        v.symbol.updateInfo(karaType)
        val applyTpt = AppliedTypeTree(Ident(karaClass), List(v.tpt))
        val resultTree = v.copy(tpt = applyTpt, rhs = applyTree).setSymbol(v.symbol)
        Right(cmp.localTyper.typed { resultTree })
      /*
       * The following case changes assignments to writes
       */
      case Apply(Select(_, name), Nil) if(name == readName) => Right(tree)
      case Assign(lhs, rhs) if (hasAnnotation(lhs, karaAnnotation)) =>
        val retypedLhs = cmp.localTyper.typed { resetLocalAttrs(lhs) }
        val newRhs = transform(cmp, rhs) match {
          case Left(x) => x
          case Right(x) => x
        }
        val resultTree = Apply(Select(retypedLhs, writeName), List(newRhs))
        Right(cmp.localTyper.typed(resultTree))
      case id : Ident if (hasAnnotation(id, karaAnnotation)) =>
        val retypedId = cmp.localTyper.typed { resetLocalAttrs(id) }
        val resultTree = Select(retypedId, readName)
        Right(cmp.localTyper.typed { resultTree })
      case select @ Select(This(_), _) if (hasAnnotation(select, karaAnnotation)) =>
        val retypedSelect = cmp.localTyper.typed { resetLocalAttrs(select) }
        val resultTree = Select(retypedSelect, readName)
        Right(cmp.localTyper.typed { resultTree })
      case x if hasAnnotation(x, karaAnnotation) =>
        throw new AssertionError(s"@incremental annotation can only appear on variables: ${x}")
      case _ => Left(tree)
    }
  }
  
  
}
