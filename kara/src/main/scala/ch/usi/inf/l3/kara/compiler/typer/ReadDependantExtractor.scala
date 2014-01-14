package ch.usi.inf.l3.kara.compiler.typer

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.kara.KaraPlugin
import ch.usi.inf.l3.kara.quals.incremental
import ch.usi.inf.l3.kara.runtime.KaraVariable
import ch.usi.inf.l3.lombrello.util.Utilities
import scala.reflect.internal.Flags._

/*
 * TODO: There exist numerous methods and functionalities that need to be put
 * in the Lombrello framework. Do it ASAP -- Amanj
 */
class ReadDependantExtractor(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some("kara-typer")
  val runsAfter = List[String]("kara-typer")
  override val runsBefore = List[String](plgn.utilities.PHASE_JVM)
  val phaseName = "kara-mover"
  import plugin.global._
  import plugin._

  val karaClass = rootMirror.getClassByName(newTypeName(s"${plgn.karaClassName}"))
  val karaModule = karaClass.companionModule
  val applyName = newTermName("apply")
  val readName = newTermName("read")
  val writeName = newTermName("write")

  definitions.PredefModule
  val incAnno = getAnnotation(plgn.karaAnnotationName)

  /*
   * TODO: The following three functions are directly copied from Mina plugin,
   * they are all odd/old and wordy, I should organize them better and put my 
   * current knowledge of Scala compiler plugin writing and put them here.
   * I also need to integrate it into Lombrello framework itself. -- Amanj
   */
  private def findSymbol(name: Name, tpe: Type, paramSyms: List[Symbol]): Option[Symbol] = {
    val r = for (
      p <- paramSyms if (p.name == name && p.tpe =:= tpe)
    ) yield {
      p
    }
    r match {
      case head :: Nil => Some(head)
      case _ => None
    }
  }
  private def changeOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): Unit = {
    var list: Map[Symbol, Symbol] = Map.empty
    tree.foreach {
      (x: Tree) =>
        {
          if (goodSymbol(x.symbol)
            && x.symbol.owner == oldOwner) {
            x match {
              case ident: Ident if list.contains(ident.symbol) => ident.symbol = list(ident.symbol)
              case ident: Ident => ()
              case _ =>
                val ts = x.symbol
                val ns = x.symbol.cloneSymbol(newOwner)
                x.symbol = ns
                list = list + (ts -> ns)
                changeOwner(x, ts, ns)
            }
          }
        }
    }
  }

  private def changeParamSymbols(tree: Tree, oldOwner: Symbol, paramSyms: List[Symbol]): Unit = {
    tree.foreach {
      (x: Tree) =>
        {
          if (x.symbol != null && x.symbol != NoSymbol
            && x.symbol.owner == oldOwner) {
            val ns = findSymbol(x.symbol.name, x.symbol.tpe, paramSyms)
            x.symbol = ns match {
              case Some(s) => s
              case None => x.symbol
            }
          }
        }
    }
  }

  /**
   * This function takes care to safely extract a method whenever it sees a call
   * to read method of KaraVariable.
   *
   * Currently it extracts methods upon calls to read method per statement,
   * i.e. a method like the following will be split into two methods not three:
   *
   * {{{
   *  // Suppose that v and y is a self-adjusted variable
   *  def m(): Unit = {
   *  	v.write(v.read + "hello" + y.read)
   *  }
   * }}}
   */
  def traverseAndExtract(cmp: TransformerComponent, tree: Tree): Either[Tree, (Block, Block)] = {
    tree match {
      case Block(Nil, expr) => Left(tree)
      case Block(stmts, expr) =>
        val (fst, snd) = stmts.splitAt(
          stmts.indexWhere((x) => x.exists((y) => y match {
            case Select(qual, name) if (name == readName) => true
            case _ => false
          })) + 1)
        (fst, snd) match {
          case (Nil, rest) => Left(tree) 
          case (xs, ys) => 
            val fst = Block(xs.dropRight(1), xs.last)
            val snd = Block(ys, expr)
            Right(fst, snd)
        }

      case t => Left(t)
    }
  }

  def extractDefDef(cmp: TransformerComponent, mthd: DefDef): (DefDef, Option[DefDef]) = {
    val vparamss = mthd.vparamss
    val rhs = mthd.rhs
    val tparams = mthd.tparams
    if (rhs.exists((x) => goodSymbol(x.symbol) && x.symbol.name == readName)) {
      // TODO you should extract out from where the read exists to the end 
      // of the method

      // challenges? the extraction should take care of all the variables 
      // that is initialized earlier in this method and is used later after 
      // the read
      // It is easy to handle the cases variables, but what happens if the 
      // defines inner methods and classes that are going to be used later?
      // For defs, you can pass them as method parameters, but what about
      // classes?
      val mthdSymbol = mthd.symbol
      val mthdOwner = mthd.symbol.owner
      traverseAndExtract(cmp, rhs) match {
        case Left(t) => (mthd, None)
        case Right((b1 @ Block(_, _), b2 @ Block(_, _))) => // do things
          var ownedIdents = List.empty[Tree]
          b2.foreach((x) =>
            if (x.isInstanceOf[Ident] && x.symbol.owner == mthdSymbol
                && !ownedIdents.exists((y) => x.symbol == y.symbol)) 
              ownedIdents = x :: ownedIdents)
          val baseOwnedIdents = ownedIdents.foldLeft(List.empty[Tree])((z, y) =>
            mthd.exists((x) => x.isInstanceOf[ValDef] && x.symbol == y.symbol
                && !b2.exists((k) => k.isInstanceOf[ValDef] && k.symbol == y.symbol)) match {
              case true => y :: z
              case false => z
            })


          val newMthdSymbol = mthdOwner.newMethodSymbol(
            cmp.unit.freshTermName(mthdSymbol.name.toString),
            mthdOwner.pos.focus, mthdSymbol.flags)

          val paramSyms = baseOwnedIdents.map(
            (x) => {
              val temp = x.symbol.cloneSymbol(newMthdSymbol, x.symbol.flags)
              temp.info = x.symbol.info
              temp
            })

          val newParams = paramSyms.map((x) => ValDef(x, EmptyTree))
          val args = newParams.foldLeft(List.empty[Tree])((z, y) =>
            Ident(baseOwnedIdents.find((x) => y.name == x.symbol.name).get.symbol) :: z).reverse

          val mthdTpe = MethodType(paramSyms, mthdSymbol.tpe)

          val newTparams = tparams.filter((t) => paramSyms.exists(t.symbol.info =:= _.info))
          newMthdSymbol.setInfoAndEnter(mthdTpe)
          val newTargs = newTparams.foldLeft(List.empty[Tree])((z, y) =>
            Ident(tparams.find((x) => y.tpe == x.symbol.tpe).get.symbol) :: z).reverse

          changeParamSymbols(b2, mthdSymbol, paramSyms)
          changeOwner(b2, mthdSymbol, newMthdSymbol)

          val newMthdTree = DefDef(Modifiers(newMthdSymbol.flags),
            newMthdSymbol.name, newTparams, List(newParams), mthd.tpt, b2).setSymbol(newMthdSymbol)

          val typedMthdTree = cmp.localTyper.typed { newMthdTree }.asInstanceOf[DefDef]

          val toCall = mthdOwner.isClass match {
            case true => Select(This(mthdOwner), newMthdSymbol)
            case false => Select(Ident(mthdOwner), newMthdSymbol)
          }
          val newApply = newTparams match {
            case Nil => cmp.localTyper.typed { Apply(toCall, args) }
            case _ =>
              cmp.localTyper.typed { Apply(TypeApply(toCall, newTargs), args) }
          }
          val newBaseRhs = Block(b1.stats ++ List(b1.expr), newApply)
          val baseMthd = cmp.localTyper.typed{
            mthd.copy(rhs = newBaseRhs).setSymbol(mthdSymbol)
          }.asInstanceOf[DefDef]
          (baseMthd, Some(typedMthdTree))
      }
    } else
      (mthd, None)

  }
  def doTransform(cmp: TransformerComponent, tree: Template): Template = {
    val newBody = tree.body.foldLeft(List.empty[Tree])((z, y) =>
      y match {
        case x: DefDef => 
          val (original, extracted) = extractDefDef(cmp, x)
          original :: extracted.toList ++ z
        case _ => y :: z
      }
    )
    tree.copy(body = newBody)
  }
  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    tree match {
      case clazz @ ClassDef(mods, name, tparamss, impl) =>
        val newImpl = doTransform(cmp, impl)
        val newClazz = clazz.copy(impl = newImpl).setSymbol(clazz.symbol)
        Right(cmp.localTyper.typed { newClazz })
      case module @ ModuleDef(mods, name, impl) =>
        val newImpl = doTransform(cmp, impl)
        val newModule = module.copy(impl = newImpl).setSymbol(module.symbol)
        Right(cmp.localTyper.typed { newModule })
      case _ => Left(tree)
    }
  }

}
