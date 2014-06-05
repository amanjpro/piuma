/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.kara.compiler

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.kara.KaraPlugin
import ch.usi.inf.l3.kara.quals._
import ch.usi.inf.l3.kara.runtime.KaraVariable

class Simplifier(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
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

  def karaApply(tpt: Tree, arg: Tree): Tree = {
    arg match {
      case EmptyTree => EmptyTree
      case _ => Apply(TypeApply(Select(Ident(karaModule), applyName), List(tpt)), List(arg))
    }
  }
  
  private def hasRead(x: Tree): Boolean = {
    x.exists((y) => y match {
      case Select(qual, name) if (name == readName) => true
      case _ => false
    })
  }
  
  private def hasRead(xs: List[Tree]): Boolean = {
    xs.foldLeft(false)((z, x) => z || hasRead(x))
  }
  
  def transformValDef(cmp: TransformerComponent, v: ValDef): ValDef = {
    assert(v.symbol.isValueParameter || v.symbol.isLocal,
      s"""|Only local variables and method are allowed to be @incremental,
                        |therefore you cannot self adjust: ${v}""".stripMargin)
    val karaType = TypeRef(NoPrefix, karaClass, List(v.symbol.info))
    val applyTree = karaApply(v.tpt, v.rhs)
    v.symbol.updateInfo(karaType)
    val applyTpt = AppliedTypeTree(Ident(karaClass), List(v.tpt))
    val resultTree = v.copy(tpt = applyTpt, rhs = applyTree).setSymbol(v.symbol)
    cmp.localTyper.typed { resultTree }.asInstanceOf[ValDef]
  }
  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    tree match {
      case b @ Block(stats, expr) if hasRead(stats) || hasRead(expr) =>
        val newStats = stats.foldLeft(List.empty[Tree])((z, y) => {
          hasRead(y) match {
            case false => z ++ List(y)
            case _ => z
          }
        })
        Right(b)
      case t => Left(t)
    }
  }
}
