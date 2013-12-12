package ch.usi.inf.l3.seal.compiler.typer

import ch.usi.inf.l3.lombrello.transform.dsl._
import ch.usi.inf.l3.seal.ScalaSealPlugin
import ch.usi.inf.l3.seal.quals.incremental

class ScalaSealTyper(val plgn: ScalaSealPlugin) extends TransformerPluginComponent(plgn) {
  override val runsRightAfter = Some(plgn.utilities.PHASE_SUPERACCESSORS)
  val runsAfter = List[String](plgn.utilities.PHASE_FLATTEN, plgn.utilities.PHASE_PATMAT, plgn.utilities.PHASE_TYPER)
  override val runsBefore = List[String](plgn.utilities.PHASE_ICODE)
  val phaseName = "test"
  import plugin.global._
  import plugin._

  var selfAdjustedClasses = Map.empty[Symbol, Tree]
  var selfAdjustedVars = StoreScope[Tree]
  
  private def enterScope: Unit = selfAdjustedVars = selfAdjustedVars.enterScope
  private def exitScope: Unit = selfAdjustedVars = selfAdjustedVars.exitScope
  val incAnno = getAnnotation("seal.quals.incremental")

  private def transformBlock(tree: Tree)(implicit plgn: TransformerComponent): Tree = {
    enterScope
    val t = transform(plgn, tree)
    exitScope
    t
  }
  /*
   * TODO
   * implement this class
   */
  private def selfAdjust(t: Tree): Tree = t
  
  def transform(implicit cmp: TransformerComponent, tree: Tree): Tree = {
    tree match {
      case x: ValDef if hasAnnotation(x, incAnno) =>
        assert(isVar(x) && isFinal(x),
          s"""|Only variables with final qualifier is allowed to be @incremental,
                |therefore you cannot self adjust: ${x}""".stripMargin)
        val clazz = selfAdjustedClasses.get(x.tpt.symbol) match {
          case None =>
            val classOption = getClassTree(x.tpt.symbol)
            assert(classOption != None, s"""|x{x} cannot be self adjusted, ${x.tpt}
               |is not available in the current compilation unit""".stripMargin)
            val c = classOption.get 
            val newClazz = cmp.duplicate(c, cmp.unit.freshTypeName(c.symbol.name.toString))
            val adjustedClass = selfAdjust(newClazz)
            selfAdjustedClasses = selfAdjustedClasses + (x.tpt.symbol -> adjustedClass)
          case Some(t) => t
        }
        selfAdjustedVars.decl(x.symbol, x)
        x
      case x @ Block(stmts, expr) => {
        enterScope
        val nStmts = stmts.map(transform(cmp, _))
        val nExpr = transform(cmp, expr)
        exitScope
        Block(nStmts, nExpr)
      }
//      case x: ValDef if isVar(x) && hasAnnotation(x, incAnno) =>
//        /*
//      	 * TODO:
//      	 * duplicate the class of this var, and change the tpe of this var to 
//      	 * the newly created type.
//      	 * Then make the newly created type an incremental computed type
//      	 */
//        val clazz = getClassTree(x.tpt.symbol).get
//        val newClazz = cmp.duplicate(clazz, cmp.unit.freshTypeName(clazz.symbol.name.toString))
//        tree
//      case x: ValDef if isVar(x) =>
//        hasAnnotation(x.tpt, incAnno) match {
//          case true =>
//            x.symbol.addAnnotation(incAnno)
//            x
//          case false =>
//            x
//        }
//      case x: ClassDef if hasAnnotation(x, incAnno) =>
//        /*
//         * TODO
//         * Change x to an incremental computed type
//         */
//        x
//      case x: ModuleDef if hasAnnotation(x, incAnno) =>
//        /*
//         * TODO
//         * Change x to an incremental computed type
//         */
//        x
//      case x: ModuleDef if hasSymbol(x) =>
//        inferIncrementalAnnotation(x.symbol)
//        hasAnnotation(x, incAnno) match {
//          case true =>
//            /*
//		     * TODO
//    		 * Change x to an incremental computed type
//		     */
//            x
//          case false =>
//            x
//        }
      case _ => tree
    }
  }

  private def inferIncrementalAnnotation(clazz: Symbol): Unit = {
    val x = clazz.parentSymbols.foldLeft(true)((x: Boolean, y: Symbol) => x && hasAnnotation(y, incAnno))
    x match {
      case true =>
        clazz.addAnnotation(incAnno)
      case false =>
        inferFromParents(clazz.parentSymbols) match {
          case true =>
            clazz.addAnnotation(incAnno)
          case false => ()
        }
    }
  }

  private def inferFromParents(clazzes: List[Symbol]): Boolean = {
    clazzes match {
      case Nil => false
      case x :: xs =>
        if (hasAnnotation(x, incAnno)) true
        else if (inferFromParents(x.parentSymbols)) {
          x.addAnnotation(incAnno)
          true
        } else inferFromParents(xs)
    }
  }
}
