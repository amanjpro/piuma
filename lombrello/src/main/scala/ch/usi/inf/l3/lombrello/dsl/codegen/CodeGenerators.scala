package ch.usi.inf.l3.lombrello.dsl.codegen



/**
 * @author Amanj Sherwany
 * @date 8 May 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.parser
import lombrello.dsl.source._
import scala.annotation.tailrec

trait CodeGenerators { self: Compiler =>

  class CodeGenerator extends Phase {
    type InputType = self.Tree
    type OutputType = List[CompiledCode]

    val name: String = "codegen"
    val runsAfter: Option[String] = Some("parser")


    private val TABSIZE: Int = 2

    def run(tree: InputType): OutputType = {
      tree match {
        case Program(trees) => 
          trees.map(generateCode(_))
          Nil
        case _ => Nil
      }
    }

    // TODO: make this tailrec
    // TODO: and implement it
    private def generateCode(tree: Tree): CompiledCode = {
      tree match {
        case PackageDef(pid, trees, _) =>
          val pname = codegen(pid)
          val pkgPath = pname.replaceAll(".", java.io.File.separator)
          val pkg = s"package ${pname}"
          null
        case _ => null
      } 
    }


    @tailrec private def genSeq(trees: List[Tree], opener: String,
      closer: String, level: Int = 0, sep: String = ",",
      collected: List[String] = Nil): String = {
      trees match {
        case Nil if collected == Nil => ""
        case Nil => 
          val reversed = collected.reverse
          s"${opener}${reversed.mkString(sep)}${closer}"
        case x :: xs =>
          genSeq(xs, opener, closer, level, sep, codegen(x) :: collected)
      }
    }


    private def codegen(tree: Tree, level: Int = 0): String = {
      tree match {
        case Ident(name, _) => pad(name, level)
        case Select(qual, id, _) =>
          pad(s"${codegen(qual)}.${codegen(id)}", level)
        case DefDef(mod, name, Nil, Nil, tpe, rhs, _) if mod.isParam =>
          val r = s"${codegen(name)}: ${codegen(tpe)}"
          pad(r, level)
        case DefDef(mod, name, Nil, Nil, tpe, rhs, _) =>
          val r = s"${mod} val ${codegen(name)}: ${codegen(tpe)} =\n"
          pad(r + s"${codegen(rhs, level + 1)}", level)
        case DefDef(mod, name, tparams, params, tpe, rhs, _) =>
          val r1 = s"${mod} val ${codegen(name)}${genSeq(tparams, "[", "]")}"
          val r2 = s"${genSeq(params, "(", ")")}"
          val r3 = s"${codegen(tpe)} =\n${codegen(rhs, level + 1)}}"
          pad(r1 + r2 + r3, level)
        case TParamDef(name, lbound, ubound, _) =>
          val nme = codegen(name)
          val lb = codegen(lbound)
          val ub = codegen(ubound)
          pad(s"${nme} >: ${lb} <: ${ub}", level)
        case SimpleType(id, tparams, _) =>
          val nme = codegen(id)
          val tps = genSeq(tparams, "[", "]")
          pad(s"${nme}${tps}", level)
        case ProductType(tpes, _) =>
          pad(genSeq(tpes, "(", ")"), level)
        case FunctionType(tpes, ret, _) =>
          val curries = genSeq(tpes, "(", ")")
          val r = codegen(ret)
          pad(s"${curries} => ${r}", level)
        case Block(stats, expr, _) => 
          // TODO
          ""
        case Function(params, rhs, _) =>
          val ps = genSeq(params, "(", ")")
          val rs = codegen(rhs)
          pad(s"${ps} => ${rs}", level)
        case Apply(m, targs, args, _) =>
          val ms = codegen(m)
          val ts = genSeq(targs, "[", "]")
          val as = genSeq(args, "(", ")")
          pad(s"${ms}${ts}${as}", level)
        case l: Literal => pad(l.litRep, level)
        case Match(cond, cases, _) =>
          val cs = s"${codegen(cond, level)} match {\n"
          val cses = genSeq(cases, "", "", level + 1, "\n")
          val end = pad("}", level)
          s"${cs}${cses}\n${end}"
        case CaseDef(p, Some(cond), rhs, _) =>
          val cse = pad(s"case ${codegen(p)} if(${codegen(cond)}) =>\n", level)
          val body = codegen(rhs, level + 1)
          cse + body
        case CaseDef(p, None, rhs, _) =>
          val cse = pad(s"case ${codegen(p)} =>\n", level)
          val body = codegen(rhs, level + 1)
          cse + body
        case Bind(id, tpe, Nil, _) =>
          val ids = codegen(id)
          val tps = codegen(tpe)
          s"${ids}: ${tpe}"
        case Bind(id, tpe, pats, _) =>
          val ids = codegen(id)
          val tps = codegen(tpe)
          val ps = genSeq(pats, "(", ")")
          s"${ids} @ ${tpe}${ps}"
        case LiteralPattern(l, _) =>
          codegen(l)
        case If(cond, thenp, elsep, _) =>
          val ifconds = pad(s"if(${codegen(cond)}) \n", level)
          val thenps = codegen(thenp, level + 1)
          val elseps = codegen(elsep, level + 1)
          s"${ifconds}\n${thenps}\nelse\n${elsep}"
        case Try(body, catches, Some(expr), _) =>
          val ts =  pad("try", level)
          val bs = codegen(body, level + 1)
          val cs = genSeq(catches, "", "", level + 1, "\n")
          val fs = codegen(expr)
          s"${ts}\n${bs}catch {\n${cs}\n} fainlly ${fs}"
        case Try(body, catches, None, _) =>
          val ts =  pad("try", level)
          val bs = codegen(body, level + 1)
          val cs = genSeq(catches, "", "", level + 1, "\n")
          s"${ts}\n${bs}catch {\n${cs}\n}"
        case Binary(lhs, op, rhs, _) =>
          pad(s"(${codegen(lhs)}) ${op.toString} (${codegen(rhs)})", level)
        case Unary(op, oprnd, _) =>
          pad(s"${op.toString} (${codegen(oprnd)})", level)
        case Record(values, _) =>
          pad(genSeq(values, "(", ")"), level)
        case New(tpe, args, _) =>
          val ts = codegen(tpe)
          val as = genSeq(args, "(", ")")
          pad(s"new ${ts}${as}", level)
        case Throw(exp, _) =>
          val es = codegen(exp)
          pad(s"throw ${es}", level)
        case PropertyTree(RunsBeforeProperty, value, pos) =>
          pad(s"override val runsBefore: List[String] = ${codegen(value)};", level)
        case PropertyTree(RunsAfterProperty, value, pos) =>
          pad(s"val runsAfter: List[String] = ${codegen(value)};", level)
        case PropertyTree(RunsRightAfterProperty, value, pos) =>
          val r = "override val runsRightAfter: Option[String] = " +
                  s"${codegen(value)};"
          pad(r, level)
        case PropertyTree(NoProperty, value, pos) =>
          ""
      }
    }


    private def pad(str: String, tabs: Int): String = {
      (" " * (tabs * TABSIZE)) + str
    }
  }
}
