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

    private val additionalImports = 
                "import ch.usi.inf.l3.lombrello.transform.dsl._\n" +
                "import ch.usi.inf.l3.lombrello.util._\n"

    val name: String = "codegen"
    val runsAfter: Option[String] = Some("typer")


    private val TABSIZE: Int = 2

    def run(tree: InputType): OutputType = {
      tree match {
        case Program(trees) => 
          trees.map(generateCode(_)).flatten
        case _ => Nil
      }
    }

    private def generateCode(tree: Tree): List[CompiledCode] = {
      tree match {
        case PackageDef(pid, trees, _) =>
          val pname = codegen(pid)
          val sep = java.io.File.separator
          val pkgPath = pname.replace(".", sep)
          val pkg = s"package ${pname}\n\n\n"
          val classes = codegen(trees, pkg, Map.empty, "")
          classes.map((x) => {
            new CompiledCode(s"${pkgPath}${sep}${x._1}.scala", x._2)
          }).toList
        case _ => Nil
      } 
    }


    @tailrec private def codegen(trees: List[Tree], preamble: String, 
          collected: Map[String, String], plgn: String): Map[String, String]  = {
      trees match {
        case Nil => collected
        case Import(id, _) :: xs => 
          codegen(xs, preamble + s"import ${codegen(id)}\n", collected, plgn)
        case (x: PhaseDef) :: xs =>
          val (r, ph) = codegenPhase(x, plgn, preamble) 
          codegen(xs, preamble, collected + (ph -> r), plgn)
        case (x: PluginDef) :: xs =>
          val (r, pn) = codegenPlugin(x, preamble) 
          codegen(xs, preamble, collected + (pn -> r), pn)
        case x :: xs =>
          codegen(xs, preamble, collected, plgn)
      }
    }

    private def codegenPlugin(plugin: PluginDef, preamble: String): 
          (String, String) = {
      val plgnName = codegen(plugin.name)
      val header = s"final class ${plgnName}"+
            "(override val global: TGlobal) extends TransformerPlugin(global)"
      val components = {
        val temp = plugin.phases.foldLeft("")((z, y) => {
          pad(s"${z}\nnew ${codegen(y)}(this),", 1)
        })
        pad("List[TransformerPluginComponent] = List(" +
          s"${temp.substring(0, temp.length -1)})", 1)
      }
      val defs = plugin.body.foldLeft("")((z, y) => {
        s"${z}\n${codegen(y, 1)}"
      })
      
      val r = s"""|
      |${preamble} 
      |${additionalImports} 
      |
      |
      |
      |${header} {
      |${components}
      |
      |${defs}
      |}
      """.stripMargin
      (r, plgnName)
    }

    private def codegenPhase(phase: PhaseDef, 
            plgn: String, preamble: String): (String, String) = {
      val phaseName = codegen(phase.name)
      val header = s"final class ${phaseName}"+
            s"(val plgn: ${plgn}) extends TransformerPluginComponent(plgn)"
      val nme = pad(s"""val phaseName = "${phase.name}"""", 1)
      val properties = phase.preamble.foldLeft("")((z, y) =>{
        s"${z}${codegen(y, 1)}\n"
      })
      val moreImports = pad("import plugin.global._\nimport plugin._\n", 1)
      val action = codegenPerform(phase.perform, phase.isChecker)
      
      val body = phase.body.foldLeft("")((z, y) => {
        s"${z}\n${codegen(y, 1)}"
      })
      val r = s"""|
      |${preamble} 
      |${additionalImports} 
      |
      |
      |
      |${header} {
      |
      |${properties}
      |
      |${moreImports}
      |
      |${action}
      |
      |${body}
      |
      |}
      """.stripMargin
      (r, phaseName)
    }

    private def codegenPerform(action: DefDef, isChecker: Boolean): String = {
      val h = isChecker match {
        //TODO fix this
        case true =>
          pad("def transform(cmp: TransformComponent, " + 
          "tree: Tree): Either[Tree, Tree] = ", 1)
        case false =>
          pad("def transform(cmp: TransformComponent, " + 
          "tree: Tree): Either[Tree, Tree] = ", 1)
      }
      s"${h}\n${codegen(action.rhs, 1)}"
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
        case Ident(name, _, _) => pad(name, level)
        case Select(qual, id, _, _) =>
          pad(s"${codegen(qual)}.${codegen(id)}", level)
        case DefDef(mod, name, Nil, Nil, tpe, rhs, _, _) if mod.isParam =>
          val byname = if(mod.isByName) " => " else ""
          val r = s"${codegen(name)}: ${byname} ${codegen(tpe)}"
          pad(r, level)
        case DefDef(mod, name, Nil, Nil, tpe, rhs, _, _) if mod.isVariable =>
          val r = s"private val ${codegen(name)}: ${codegen(tpe)} =\n"
          pad(r + s"${codegen(rhs, level + 1)}", level)
        case DefDef(mod, name, tparams, params, tpe, rhs, _, _) =>
          val r1 = s"private def ${codegen(name)}${genSeq(tparams, "[", "]")}"
          val r2 = s"${genSeq(params, "(", ")")}:"
          val r3 = s"${codegen(tpe)} =\n${codegen(rhs, level + 1)}"
          pad(r1 + r2 + r3, level)
        case TParamDef(name, lbound, ubound, _, _) =>
          val nme = codegen(name)
          val lb = codegen(lbound)
          val ub = codegen(ubound)
          pad(s"${nme} >: ${lb} <: ${ub}", level)
        case SimpleTypeTree(id, tparams, _, _) =>
          val nme = codegen(id)
          val tps = genSeq(tparams, "[", "]")
          pad(s"${nme}${tps}", level)
        case ProductTypeTree(tpes, _, _) =>
          pad(genSeq(tpes, "(", ")"), level)
        case FunctionTypeTree(tpes, ret, _, _) =>
          val curries = genSeq(tpes, "(", ")")
          val r = codegen(ret)
          pad(s"${curries} => ${r}", level)
        case Block(stats, expr, _) => 
          val stmts = genSeq(stats, "", "", level + 1, "\n")
          val es = pad(codegen(expr), level + 1)
          val open = pad("{\n", level)
          val close = pad("}", level)
          s"${open}${stmts}${es}\n${close}"
        case Function(params, rhs, _) =>
          val ps = genSeq(params, "(", ")")
          val rs = codegen(rhs)
          pad(s"${ps} => ${rs}", level)
        case Apply(m, targs, args, _, _) =>
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
        case Bind(id, tpe, Nil, _, _) =>
          val ids = codegen(id)
          val tps = codegen(tpe)
          s"${ids}: ${tpe}"
        case Bind(id, tpe, pats, _, _) =>
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
        case New(tpe, args, _, _) =>
          val ts = codegen(tpe)
          val as = genSeq(args, "(", ")")
          pad(s"new ${ts}${as}", level)
        case Throw(exp, _, _) =>
          val es = codegen(exp)
          pad(s"throw ${es}", level)
        case This(_, _) =>
          pad("this", level)
        case Super(_, _) =>
          pad("super", level)
        case PropertyTree(RunsBeforeProperty, value, pos) =>
          pad(s"override val runsBefore: List[String] = ${codegen(value)}", level)
        case PropertyTree(RunsAfterProperty, value, pos) =>
          pad(s"val runsAfter: List[String] = ${codegen(value)}", level)
        case PropertyTree(RunsRightAfterProperty, value, pos) =>
          val r = "override val runsRightAfter: Option[String] = " +
                  s"${codegen(value)}"
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
