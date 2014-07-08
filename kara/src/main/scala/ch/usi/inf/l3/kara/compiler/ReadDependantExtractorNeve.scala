/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.kara.compiler


import ch.usi.inf.l3.lombrello.neve.NeveDSL._
import scala.reflect.internal.Flags._
import scala.Option.option2Iterable
import ch.usi.inf.l3.kara.KaraPlugin

/*
 * TODO: There exist numerous methods and functionalities that need to be put
 * in the Lombrello framework. Do it ASAP -- Amanj
 */
@phase("kara-mover") class ReadDependantExtractorNeve {
  //  override val runsRightAfter = Some("kara-typer")
  //  val runsAfter = List[String]("kara-typer")
  //  override val runsRightAfter = Some("classFinder")
  after(List("classFinder"))

  before(List(PHASE_PATMAT))


  val karaClassName = "ch.usi.inf.l3.kara.runtime.KaraVariable"
  val karaRuntimeStr = "ch.usi.inf.l3.kara.runtime.KaraRuntime"
  val karaAnnotationName = "ch.usi.inf.l3.kara.quals.incremental"



  val karaClass = 
      rootMirror.getClassByName(newTypeName(karaClassName))
  val karaRuntime = 
      rootMirror.getClassByName(newTypeName(
                  karaRuntimeStr)).companionModule
  val readName = newTermName("read")
  val runClosureName = newTermName("runClosure")
  val karaAnnotation = getAnnotation(karaAnnotationName)


  private def hasRead(x: Tree): Boolean = {
    // As for If and Match, shall I announce ``read'' if only the condition has
    // it? mmm makes sense to some extend, but not sure
    val b = new TreeTraverserPredicator {
      def traverseIdent(id: Ident): Boolean = {
        hasAnnotation(id, karaAnnotation)
      }

      override def otherwise(t: Tree): Boolean = false

      override def traverseAssign(t: Assign): Boolean = {
        traverse(t.rhs)
      }

      override def traverseClassDef(t: ClassDef): Boolean = {
        traverse(t.impl)
      }

      override def traverseModuleDef(t: ModuleDef): Boolean = {
        traverse(t.impl)
      }

      override def traverseDefDef(t: DefDef): Boolean = {
        traverse(t.rhs)
      }

      override def traverseFunction(t: Function): Boolean = {
        traverse(t.body)
      }

      override def traverseLabelDef(t: LabelDef): Boolean = {
        traverse(t.rhs)
      }

      override def traverseSelect(t: Select): Boolean = {
        hasAnnotation(t, karaAnnotation) || traverse(t.qualifier)
      }

      override def traverseTemplate(t: Template): Boolean = {
        traverse(t.body)
      }

      override def traverseValDef(t: ValDef): Boolean = {
        traverse(t.rhs)
      }
    }

    b(x)

  }

  private def doTransform(tree: Template): Template = {
    def traverseDefDef(mthds: List[DefDef]): List[DefDef] = {
      mthds match {
        case Nil => Nil
        case x :: xs =>
          x.rhs match {
            case rhs: Block =>
              val (fst, snd) = splitAfter(rhs, (y: Tree) => hasRead(y))
              val r = extractMethod(snd, x.symbol, freshName(x.name))

              r match {
                case Some((sndDef, apply)) =>
                  val args = apply.args

                  val closure = def2Function(sndDef, sndDef.symbol, apply)

                  val newArgs = mkLiteral(sndDef.symbol.fullName) :: closure :: args
                  
                  val types = args.map {
                    (x) => TypeTree(x.tpe)
                  } ++ List(TypeTree(sndDef.tpt.tpe))

                  val fun = Select(Ident(karaRuntime), runClosureName)

                  val typedApply = mkApply(fun, types, newArgs)
                  
                  val fstRhs = Block(fst, typedApply)

                  val fstDef = mkDefDef(x.name, x.tparams, 
                                x.vparamss, x.tpt, fstRhs, x.symbol)
                  fstDef :: sndDef :: traverseDefDef(xs)
                case None =>
                  x :: traverseDefDef(xs)
              }
            case _ =>
              x :: traverseDefDef(xs)
          }
        }
    }

    val newBody = tree.body.foldLeft(List.empty[Tree])((z, y) =>
      y match {
        case x: DefDef =>
          val traversed = traverseDefDef(List(x))
          traversed ++ z
        case _ => y :: z
      })
    val tmpl = treeCopy.Template(tree, tree.parents, tree.self, newBody)
    tmpl
  }
  def transform(tree: Tree): Tree = {
    tree match {
      case clazz @ ClassDef(mods, name, tparams, impl) if hasRead(clazz) =>
        val newImpl = doTransform(impl)
        val newClazz = treeCopy.ClassDef(clazz, mods, name, tparams, newImpl)
        super.transform(localTyper.typed { newClazz })
      case module @ ModuleDef(mods, name, impl) if hasRead(module)=>
        val newImpl = doTransform(impl)
        val newModule = treeCopy.ModuleDef(module, mods, name, newImpl)
        super.transform(localTyper.typed { newModule })
      case _ => super.transform(tree)
    }
  }

}
