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
final class ReadDependantExtractor(val plgn: KaraPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some("kara-typer")
  val runsAfter = List[String]("kara-typer")
  override val runsBefore = List[String](plgn.utilities.PHASE_JVM)
  val phaseName = "kara-mover"
  import plugin.global._
  import plugin._

  val karaClass = rootMirror.getClassByName(newTypeName(s"${plgn.karaClassName}"))
  val readName = newTermName("read")
  val incAnno = getAnnotation(plgn.karaAnnotationName)

  /**
   * This function takes care of safely extracting a method whenever it sees a
   * call to read method of KaraVariable.
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
  private def traverseAndExtract(cmp: TransformerComponent, tree: Tree): Either[Tree, (Block, Block)] = {
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

  private def extractDefDef(cmp: TransformerComponent, mthd: DefDef): (DefDef, Option[DefDef]) = {
    val vparamss = mthd.vparamss
    val rhs = mthd.rhs
    val tparams = mthd.tparams
    if (rhs.exists((x) => goodSymbol(x.symbol) && x.symbol.name == readName)) {

      /* 
       * Challenges? the extraction should take care of all the variables 
       * that is initialized earlier in this method and is used later after 
       * the read
       * It is easy to handle the cases of variables, but what happens if it 
       * defines inner methods and classes that are going to be used later?
       * 1- For defs, you can pass them as method parameters
       * 2- For classes, you can surround them with an inner def, that takes
       * the exact same variables of the classe's constructor. The def should
       * return a freshly created instance of the class. Then pass this def to 
       * the newly created method.
       * But neither of the previous two solutions work in practice, since when
       * the Kara runtime calls some kara-generated method, it fails to provide
       * these methods. 
       * -- Amanj
       **/

 
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
              newMthdSymbol.newSyntheticValueParam(x.symbol.info, x.symbol.name)
            })

          val commonTparams = tparams.filter((t) => paramSyms.exists(t.symbol.tpe =:= _.info))

          val newTargs = commonTparams.map((x) => TypeTree(x.symbol.tpe))
          val newTparamSyms = commonTparams.map(
            (x) => {
              val nsym = newMthdSymbol.newTypeParameter(cmp.unit.freshTypeName("K"), newMthdSymbol.pos.focus, x.symbol.flags)
              nsym.info = x.symbol.info
              paramSyms.foreach((y) => {
                y.substInfo(List(x.symbol), List(nsym))
              })
              nsym
            })
          val newParams = paramSyms.map((x) => ValDef(x, EmptyTree))

          val args = newParams.foldLeft(List.empty[Tree])((z, y) => {
            val id = Ident(baseOwnedIdents.find((x) => y.name == x.symbol.name).get.symbol)
            cmp.localTyper.typed(id) :: z
          }).reverse

          val newTparams = newTparamSyms.map((x) => TypeDef(x))

          val mthdTpe = newTparamSyms match {
            case Nil => MethodType(paramSyms, mthd.tpt.tpe)
            case _ => PolyType(newTparamSyms, MethodType(paramSyms, mthd.tpt.tpe))
          }
          newMthdSymbol.setInfoAndEnter(mthdTpe)

          
          cmp.fixOwner(b2, mthdSymbol, newMthdSymbol, paramSyms)
          
          val newMthdTree = DefDef(Modifiers(newMthdSymbol.flags),
            newMthdSymbol.name, newTparams, List(newParams), mthd.tpt,
            fixMutatingParams(cmp, b2)).setSymbol(newMthdSymbol)

            

          val typedMthdTree = cmp.localTyper.typed { newMthdTree }.asInstanceOf[DefDef]

          val toCall = mthdOwner.isClass match {
            case true => Select(This(mthdOwner), newMthdSymbol)
            case false => Select(Ident(mthdOwner), newMthdSymbol)
          }
          val newApply = newTparams match {
            case Nil => Apply(toCall, args)
            case _ =>
              Apply(TypeApply(toCall, newTargs), args)
          }
          val newBaseRhs = cmp.localTyper.typed { Block(b1.stats ++ List(b1.expr), newApply) }
          val untypedBase = treeCopy.DefDef(mthd, mthd.mods, mthd.name, mthd.tparams, mthd.vparamss, mthd.tpt, newBaseRhs)

          val baseMthd = cmp.localTyper.typed {
            untypedBase
          }.asInstanceOf[DefDef]
          (baseMthd, Some(typedMthdTree))
      }
    } else
      (mthd, None)

  }

  private def fixMutatingParams(cmp: TransformerComponent, tree: Block): Block = {
    var aliases: Map[Symbol, Symbol] = Map.empty

    def substituteMutation(x: Tree): Tree = {
      x match {
        case t if(aliases.contains(t.symbol)) =>
          t.setSymbol(aliases(t.symbol))
        case Assign(lhs, rhs) if (isParam(lhs.symbol)) =>
          aliases.contains(lhs.symbol) match {
            case true =>
              Assign(Ident(aliases(lhs.symbol)), rhs)
            case false =>
              val newsym = lhs.symbol.owner.newVariable(
                cmp.unit.freshTermName(lhs.symbol.name + ""),
                lhs.symbol.owner.pos.focus, MUTABLE)
              newsym.info = lhs.symbol.info
              val newValDef = ValDef(newsym, rhs)
              aliases = aliases + (lhs.symbol -> newsym)
              newValDef
          }
        case t => 
          t.substituteSymbols(aliases.keys.toList, aliases.values.toList)
          t
      }
    }
    val newStats = tree.stats.foldLeft(List.empty[Tree])((z, y) => {
      z ++ List(substituteMutation(y))
    })

    val newExpr = substituteMutation(tree.expr)

    Block(newStats, newExpr)
  }
  private def doTransform(cmp: TransformerComponent, tree: Template): Template = {
    def traverseDefDef(mthd: DefDef): List[DefDef] = {
      val (fst, snd) = extractDefDef(cmp, mthd)
      snd match {
        case None => List(fst)
        case Some(x) => fst :: traverseDefDef(x)
      }
    }
    val newBody = tree.body.foldLeft(List.empty[Tree])((z, y) =>
      y match {
        case x: DefDef =>
          val traversed = traverseDefDef(x)
          traversed ++ z
        case _ => y :: z
      })
    treeCopy.Template(tree, tree.parents, tree.self, newBody)
  }
  def transform(cmp: TransformerComponent, tree: Tree): Either[Tree, Tree] = {
    tree match {
      case clazz @ ClassDef(mods, name, tparams, impl) =>
        val newImpl = doTransform(cmp, impl)
        val newClazz = treeCopy.ClassDef(clazz, mods, name, tparams, newImpl)
        Right(cmp.localTyper.typed { newClazz })
      case module @ ModuleDef(mods, name, impl) =>
        val newImpl = doTransform(cmp, impl)
        val newModule = treeCopy.ModuleDef(module, mods, name, newImpl)
        Right(cmp.localTyper.typed { newModule })
      case _ => Left(tree)
    }
  }

}
