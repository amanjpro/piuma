package ch.usi.inf.l3.lombrello.dsl.typechecker



/**
 * @author Amanj Sherwany
 * @date 17 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.source._
import scala.annotation.tailrec
import scala.reflect.runtime.{universe => ru}
import scala.reflect.internal.MissingRequirementError

trait Refchecks { self: Compiler =>

  class Refcheck extends Phase {
    type InputType = self.Tree
    type OutputType = self.Tree
    val name: String = "refcheck"
    val runsAfter: Option[String] = Some("parser")

    def run(tree: InputType): OutputType = {
      val scalaDefaultImportQuals = box(scalaDefaultImports)
      importEliminator(tree, scalaDefaultImportQuals)
    }


    private def box(imports: List[String]): List[TypeQual] = {
      imports.map(box(_))
    }

    private def box(imprt: String): TypeQual = {
      val parts = imprt.split('.').toList
      parts.last match {
        case "_" => 
          val nparts = parts.take(parts.length - 1).mkString(".")
          TypeQual(nparts, WildcardImport)
        case _ =>
          TypeQual(imprt, ConcreteImport)
      }
    }

    /**
     * Create TypeQuals for all Imports in a PackageDef
     */
    private def firstImportCollector(pkg: PackageDef, 
            computed: List[TypeQual]): List[TypeQual] = {
      pkg.trees.foldLeft(computed)((z, y) => {
        y match {
          case x: Import => box(x.idString) :: z
          case _ =>
            z
        }
      })
    }

    /**
     * Create TypeQuals for all PhaseDefs in a PackageDef
     */
    private def firstPhaseCollector(pkg: PackageDef, 
            computed: List[TypeQual]): List[TypeQual] = {
      pkg.trees.foldLeft(computed)((z, y) => {
        y match {
          case PhaseDef(name, _, _, _, _, _, _) =>
            TypeQual(s"${pkg.pidString}.${name.name}", PhaseType) :: z
          case _ =>
            z
        }
      })
    }

    
    @tailrec private def getHighestPriorityType(nme: String, 
            imports: List[TypeQual], 
            computed: Option[TypeQual]): Option[TypeQual] = {

      def lastPart(str: String): String = {
        str.split('.').toList.last
      }
      imports match {
        case (tq @ TypeQual(qual, WildcardImport)) :: xs =>
          val defines = try {
            ru.runtimeMirror(getClass.getClassLoader).staticClass(
                    s"${qual}.${nme}")
            true
          } catch {
            case ex: MissingRequirementError =>
              try {
                ru.runtimeMirror(getClass.getClassLoader).staticClass(
                    s"${qual}.${nme}")
                true
              } catch {
                case ex: MissingRequirementError => false
              }
          }
          (defines, computed) match {
            case (false, _) =>
              getHighestPriorityType(nme, xs, computed)
            case (_, Some(c)) if c.hasHigherPriority(tq) =>
              getHighestPriorityType(nme, xs, computed)
            case _ => getHighestPriorityType(nme, xs, Some(tq))
          }
        case (tq @ TypeQual(qual, _)) :: xs 
                  if lastPart(qual) == nme =>
          computed match {
            case Some(c) if c.hasHigherPriority(tq) =>
              getHighestPriorityType(nme, xs, computed)
            case _ => getHighestPriorityType(nme, xs, Some(tq))
          }
        case x :: xs =>
          getHighestPriorityType(nme, xs, computed)
        case Nil => 
          computed match {
            case Some(TypeQual(qual, WildcardImport)) =>
              Some(TypeQual(s"${qual}.${nme}", ConcreteImport))
            case x => x
          }
      }
    }



    private def tparamHelper(tparam: TParamDef, 
            imported: List[TypeQual]): (TParamDef, List[TypeQual]) = {
      val tqual = TypeQual(tparam.name.name, DefinedType)
      val nimports = tqual :: imported
      val ntparam = 
        importEliminator(tparam, imported).asInstanceOf[TParamDef]
      (ntparam, nimports)
    }



    @tailrec private def tparamsHelper(tparams: List[TParamDef], 
            imported: List[TypeQual], 
            computed: List[TParamDef]): (List[TParamDef], List[TypeQual]) = {
      tparams match {
        case Nil => (computed.reverse, imported)
        case x :: xs =>
          val (ntparam, nimported) = tparamHelper(x, imported)
          tparamsHelper(xs, nimported, ntparam :: computed)
      }
    }

    private def toSelectOrIdent(find: Option[TypeQual], 
                or: SelectOrIdent): SelectOrIdent = {
      find match {
        case None => or
        case Some(tq) => 
          val qual = tq.qual.split('.').toList
          def toSelect(l: List[String]): SelectOrIdent = {
            l match {
              case Nil => 
                // this should not happen
                Ident(Names.DUMMY_NAME, Position())
              case a :: Nil => Ident(a, or.pos)
              case as =>
                val la = as.last
                val ba = as.take(as.length - 1)
                Select(toSelect(ba), Ident(la, or.pos), or.pos)
            }
          }
          toSelect(qual)
      }
    }

    // TODO: Convert this method to a tailrec one
    private def importEliminator(tree: Tree, imports: List[TypeQual]): Tree = {
      tree match {
        case Program(trees) =>
          Program(trees.map((x) => {
            // What about nested pakcages defined 
            // in a nother directory?
            val nimports = firstPhaseCollector(x, imports)
            importEliminator(x, nimports).asInstanceOf[PackageDef]
          }))
        case pkg @ PackageDef(pid, trees, _, _) =>
          // Classes in the current package have lower priority than
          // the imported ones but higher priority than the default
          // auto imported ones
          val nimportsTemp = firstImportCollector(pkg, imports)
          val nimports = box(pkg.pidString) :: nimportsTemp
          val ntreesTemp = trees.map(importEliminator(_, nimports))
          val ntrees = ntreesTemp.filter(_ != NoTree)
          pkg.copy(trees = ntrees)
        case defdef @ DefDef(_, _, tparams, params, tpe, rhs, _, _) =>
          val (ntparams, ctparams) = tparamsHelper(tparams, imports, Nil)

          val nimports = ctparams ++ imports
          val nparams = params.map(importEliminator(_, 
                    nimports).asInstanceOf[DefDef])
          val ntpe = importEliminator(tpe, nimports).asInstanceOf[TypeTree]
          val nrhs = importEliminator(rhs, nimports).asInstanceOf[Expression]
          defdef.copy(tparams = ntparams, params = nparams, 
                tpe = ntpe, rhs = nrhs)
        case plgn @ PluginDef(_, phases, body, _, _) => 
          val nphases = phases.map((x) => {
            val r = getHighestPriorityType(x.asString, imports, None)
            toSelectOrIdent(r, x) 
          })
          val nbody = body.map(importEliminator(_,imports).asInstanceOf[DefDef])
          plgn.copy(phases = nphases, body = nbody)
        case phs @ PhaseDef(_, _, preamble, perform, body, _, _) =>
          val npreamble = preamble.map(importEliminator(_, 
                imports).asInstanceOf[PropertyTree])
          val nperform =
            importEliminator(perform, imports).asInstanceOf[DefDef]

          val nbody = body.map(importEliminator(_, imports))

          phs.copy(preamble = npreamble, perform = nperform, body = nbody) 
        case tp @ TParamDef(_, lbound, ubound, _, _) =>
          val nlbound = importEliminator(lbound, 
                imports).asInstanceOf[TypeTree]
          val nubound = importEliminator(ubound, 
                imports).asInstanceOf[TypeTree]
          tp.copy(lbound = nlbound, ubound = nubound)
        case stt @ SimpleTypeTree(id, tparams, _, _) =>
          val ntparams = tparams.map(
              importEliminator(_, imports).asInstanceOf[TypeTree])
          val r = getHighestPriorityType(id.asString, imports, None)
          val nid = toSelectOrIdent(r, id) 
          stt.copy(id = nid, tparams = ntparams)
        case ptt @ ProductTypeTree(items, _, _) =>
          val nitems = items.map(
              importEliminator(_, imports).asInstanceOf[TypeTree])
          ptt.copy(items = nitems)
        case ftt @ FunctionTypeTree(params, ret, _, _) =>
          val nparams = params.map(
              importEliminator(_, imports).asInstanceOf[TypeTree])
          val nret = importEliminator(ret, imports).asInstanceOf[TypeTree]
          ftt.copy(params = nparams, ret = nret)
        case block @ Block(stats, expr, _) =>
          val nstats = stats.map(importEliminator(_, imports))
          val nexpr = importEliminator(expr, imports)
          block.copy(stats = nstats, expr = nexpr)
        case func @ Function(params, rhs, _) =>
          val nparams = params.map(importEliminator(_, 
                    imports).asInstanceOf[DefDef])
          val nrhs = 
            importEliminator(rhs, imports).asInstanceOf[Expression]
          func.copy(params = nparams, rhs = nrhs)
        case apply @ Apply(method, targs, args, _, _) => 
          val nmethod = importEliminator(method, 
                  imports).asInstanceOf[Expression]
          val ntargs = targs.map(importEliminator(_, 
                  imports).asInstanceOf[TypeTree])
          val nargs = targs.map(importEliminator(_, 
                  imports).asInstanceOf[Expression])
          apply.copy(method = nmethod, targs = ntargs, args = nargs)
        case select @ Select(qual, id, _, _) => select
        case id @ Ident(name, _, _) => id
        case mtch @ Match(cond, cases, _) => 
          val ncond = 
            importEliminator(cond, imports).asInstanceOf[Expression]
          val ncases = cases.map(importEliminator(_, 
            imports).asInstanceOf[CaseDef])
          mtch.copy(cond = ncond, cases = ncases)
        case casedef @ CaseDef(pattern, cond, rhs, _) => 
          val npattern = importEliminator(pattern, 
                  imports).asInstanceOf[Pattern]
          val ncond = cond match {
            case Some(c) => Some(importEliminator(c, 
                  imports).asInstanceOf[Expression])
            case _ => None
          }
          val nrhs = importEliminator(rhs, 
                  imports).asInstanceOf[Expression]
          casedef.copy(pattern = npattern, cond = ncond, rhs = nrhs)
        case bind @ Bind(id, tpe, pattern, _, _) => 
          val nid = importEliminator(id, imports).asInstanceOf[Ident]
          val ntpe = importEliminator(tpe, imports).asInstanceOf[SelectOrIdent]
          val npattern = pattern.map(importEliminator(_, 
                  imports).asInstanceOf[Pattern])
          bind.copy(id = nid, pattern = npattern, tpe = ntpe)
        case ifelse @ If(cond, thenp, elsep, _) => 
          val ncond = importEliminator(cond, imports).asInstanceOf[Expression]
          val nthenp = importEliminator(thenp, imports).asInstanceOf[Expression]
          val nelsep = importEliminator(elsep, imports).asInstanceOf[Expression]
          ifelse.copy(cond = ncond, thenp = nthenp, elsep = nelsep)
        case trycatch @ Try(cond, catches, fnly, _) => 
          val ncond = 
            importEliminator(cond, imports).asInstanceOf[Expression]
          val ncatches = catches.map(importEliminator(_, 
            imports).asInstanceOf[CaseDef])
          val nfnly = fnly match {
            case None => None
            case Some(f) => 
              Some(importEliminator(f, imports).asInstanceOf[Expression])
          }
          trycatch.copy(cond = ncond, catches = ncatches, fnly = nfnly)
        case binop @ Binary(lhs, _, rhs, _) => 
          val nlhs = 
            importEliminator(lhs, imports).asInstanceOf[Expression] 
          val nrhs = 
            importEliminator(rhs, imports).asInstanceOf[Expression] 
          binop.copy(lhs = nlhs, rhs = nrhs)
        case uniop @ Unary(_, operand, _) => 
          val noperand = importEliminator(operand, 
              imports).asInstanceOf[Expression] 
          uniop.copy(operand = noperand)
        case record @ Record(values, _) => 
          val nvalues = values.map(importEliminator(_, imports))
          record.copy(values = nvalues)
        case ptree @ PropertyTree(_, value, _) => 
          val nvalue = 
            importEliminator(value, imports).asInstanceOf[Expression]
          ptree.copy(value = nvalue)
        case newTree @ New(tpe, args, _, _) => 
          val ntpe = importEliminator(tpe, imports).asInstanceOf[SimpleTypeTree]
          val nargs = args.map(importEliminator(_, 
                    imports).asInstanceOf[Expression])
          newTree.copy(tpe = ntpe, args = nargs)
        case throwTree @ Throw(exp, _, _) => 
          val nexp = importEliminator(exp, imports).asInstanceOf[Expression]
          throwTree.copy(exp = nexp)
        case x: Import => 
          NoTree
        case x: LiteralPattern => x
        case x: This => x
        case x: Super => x
        case x : Literal => x
        case x: Comment => x
        case NoTree => NoTree
      }
    }
  }
  


  private case class TypeQual(qual: String, kind: ImportKind) {
    def hasHigherPriority(other: TypeQual): Boolean = {
      (kind, other.kind) match {
        case (DefinedType, DefinedType) => 
          false
        case (DefinedType, _) => true
        case (_, DefinedType) => false
        case (PhaseType, PhaseType) => 
          false
        case (PhaseType, _) => true
        case (_, PhaseType) => false
        case (ConcreteImport, _) => true
        case (_, ConcreteImport) => false
        case (_, _) => true
      }
    }
  }
  


  private sealed trait ImportKind
  private case object WildcardImport extends ImportKind
  private case object ConcreteImport extends ImportKind
  private case object DefinedType extends ImportKind
  private case object PhaseType extends ImportKind
}
 
