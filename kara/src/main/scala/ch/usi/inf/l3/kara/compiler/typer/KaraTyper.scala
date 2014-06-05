/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */


package ch.usi.inf.l3.kara.compiler.typer

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.kara.KaraPlugin
import ch.usi.inf.l3.kara.quals._
import ch.usi.inf.l3.kara.runtime.KaraVariable

class KaraTyper(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some("classFinder")
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
  def transformValDef(cmp: TransformerComponent, v: ValDef): ValDef = {
    assert(v.symbol.isLocal,
      s"""|Only local variables are allowed to be @incremental,
                        |therefore you cannot self adjust: ${v}""".stripMargin)
    //    assert(v.symbol.isValueParameter || v.symbol.isLocal,
    //      s"""|Only local variables and method parameters are allowed to be @incremental,
    //                        |therefore you cannot self adjust: ${v}""".stripMargin)
    val karaType = TypeRef(NoPrefix, karaClass, List(v.symbol.info))
    val applyTree = karaApply(v.tpt, v.rhs)
    v.symbol.updateInfo(karaType)
    val applyTpt = AppliedTypeTree(Ident(karaClass), List(v.tpt))
    val resultTree = v.copy(tpt = applyTpt, rhs = applyTree).setSymbol(v.symbol)
    cmp.localTyper.typed { resultTree }.asInstanceOf[ValDef]
  }
  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    tree match {
      //      case t @ Template(parents, self, body) =>
      //        val newBody = body.foldLeft(List.empty[Tree])((z, y) => {
      //          y match {
      //            case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) 
      //            	if vparamss.flatten.exists((x) => hasAnnotation(x, karaAnnotation)) =>
      //              val owner = defdef.symbol.owner
      //              val newMSymbol = owner.newMethodSymbol(name, owner.pos.focus, defdef.symbol.flags)
      //              val newParamSyms = vparamss.map((ys) => {
      //                ys.map((y) => {
      //                  val temp = newMSymbol.newValueParameter(y.name, newMSymbol.pos.focus, 
      //                		  	y.symbol.flags)
      //                  temp.setAnnotations(y.symbol.annotations)
      //                  temp.info = y.symbol.info
      //                  temp
      //                })
      //              })
      //              val newParams = newParamSyms.map((ys) => ys.map((y) => ValDef(y, EmptyTree)))
      //              val applyArgs = newParams.map((ys) => {
      //                ys.map((y) => {
      //                  hasAnnotation(y, karaAnnotation) match {
      //                    case true => 
      //                      y.symbol.removeAnnotation(karaAnnotation)
      //                      karaApply(y.tpt, y.rhs) 
      //                    case false => Ident(y.symbol)
      //                  }
      //                })
      //              }).flatten
      //              val newRhs = Apply(Ident(defdef.symbol), applyArgs) 
      //              val newDefDef = DefDef(mods, name, tparams, newParams, tpt, newRhs) 
      //              
      //              null
      //            case _ => z
      //          }
      //        })
      //        val newTemplate = treeCopy.Template(t, parents, self, newBody)
      //        Right(newTemplate)
      case v: ValDef if hasAnnotation(v, karaAnnotation) &&
        !v.symbol.isValueParameter =>
        assert(v.symbol.isLocal,
          s"""|Only local variables and method parameters are allowed to be 
            |@incremental, therefore you cannot self adjust: ${v}""".stripMargin)
        Right(v)
      /*
       * The following case changes assignments to writes
       */
//      case Apply(Select(_, name), Nil) if (name == readName) => Right(tree)
//      case Assign(lhs, rhs) if (hasAnnotation(lhs, karaAnnotation)) =>
//        val retypedLhs = cmp.localTyper.typed { resetLocalAttrs(lhs) }
//        val newRhs = transform(cmp, rhs) match {
//          case Left(x) => x
//          case Right(x) => x
//        }
//        val resultTree = Apply(Select(retypedLhs, writeName), List(newRhs))
//        Right(cmp.localTyper.typed(resultTree))
//      case id: Ident if (hasAnnotation(id, karaAnnotation)) =>
//        val retypedId = cmp.localTyper.typed { resetLocalAttrs(id) }
//        val resultTree = Select(retypedId, readName)
//        Right(cmp.localTyper.typed { resultTree })
//      case select @ Select(This(_), _) if (hasAnnotation(select, karaAnnotation)) =>
//        val retypedSelect = cmp.localTyper.typed { resetLocalAttrs(select) }
//        val resultTree = Select(retypedSelect, readName)
//        Right(cmp.localTyper.typed { resultTree })
      case x if hasAnnotation(x, karaAnnotation) =>
        throw new AssertionError(s"@incremental annotation can only appear " +
        		"on local variables and method parameters: ${x}")
      case _ => Left(tree)
    }
  }

}
