package ch.usi.inf.l3.lombrello.dsl.parser



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import lombrello.dsl.source._
import scala.annotation.tailrec

trait Parsers { self: Compiler =>


  type TokenList = List[tokens.Token]

  class Parser extends Phase {
    type InputType = List[TokenList]
    type OutputType = self.Tree
    val name: String = "parser"
    val runsAfter: Option[String] = Some("normalizer")

    def run(tokenss: InputType): OutputType = {
      val pkgs = tokenss.map(parse).foldLeft(Nil: List[PackageDef])((z, y) => {
        y match {
          case x: PackageDef => x :: z
          case _ => z
        }
      })
      Program(pkgs)
    }
    

    

    private def parse(tokenList: TokenList): Tree = {
      tokenList match {
        // case tokens.ScalaBlock(verbatim, pos) :: xs => 
          // (ScalaBlock(verbatim, pos), xs)
        // There is no comment in the parse tree
        // case tokens.CommentBlock(verbatim, pos) :: xs =>
          // (Comment(BlockComment, verbatim, pos), xs)
        // case tokens.CommentLine(verbatim, pos) :: xs =>
          // (Comment(LineComment, verbatim, pos), xs)
        case tokens.Keyword(tokens.Package) :: xs =>
          parsePackage(tokenList)
        case Nil => NoTree
        case xs =>
          val pos = posOfHead(xs)
          val pid = Ident(Names.EMPTY_PACKAGE, pos)
          val trees = parseTrees(xs)
          PackageDef(pid, trees, pos)
      }
    }
    

    private def seenPlugin(trees: List[Tree]): Boolean = {
      trees.filter(_.isInstanceOf[PluginDef]) match {
        case Nil => false
        case _ => true
      }
    }

    private def parsePackage(tokenList: TokenList): PackageDef = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Package), tokenList)
      val (pid, rest2) = parseSelectOrIdent(rest1)
      val rest3 = parseSemi(pid, rest2)
      val trees = parseTrees(rest3)
      PackageDef(pid, trees, posOfHead(tokenList))
    }
    
    @tailrec private def parseTrees(tokenList: TokenList, 
          collected: List[Tree] = Nil, canImport: Boolean = true): List[Tree] = {
      tokenList match {
        // case tokens.ScalaBlock(verbatim, pos) :: xs => 
          // (ScalaBlock(verbatim, pos), xs)
        // case tokens.CommentBlock(verbatim, pos) :: xs =>
          // Comment(BlockComment, verbatim, pos) :: parseTrees(xs)
        // case tokens.CommentLine(verbatim, pos) :: xs =>
          // Comment(LineComment, verbatim, pos) :: parseTrees(xs)
        // case tokens.Keyword(tokens.Package) :: xs =>
          // parsePackage(tokenList)
        case tokens.Keyword(tokens.Import) :: xs if canImport =>
          val (tree, rest) = parseImport(tokenList)
          parseTrees(rest, tree :: collected)
        case tokens.Keyword(tokens.Plugin) :: xs 
            if !seenPlugin(collected) =>
          val (tree, rest) = parsePlugin(tokenList)
          parseTrees(rest, tree :: collected, false)
        case tokens.Keyword(tokens.Phase) :: xs =>
          val (tree, rest) = parsePhase(tokenList)
          parseTrees(rest, tree :: collected, false)
        case Nil => Nil
          collected.reverse
        case x :: xs => 
          reporter.report(tokens.Keyword(tokens.Phase), x, BAD_TOKEN)
          Nil
      }
    }

    private def parseImport(tokenList: TokenList): (Import, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Import), tokenList)
      val (qual, rest2) = parseSelectOrIdent(rest1) match {
        case (q, tokens.Punctuation(tokens.Dot) :: xs) =>
          val r = parseOrReport(tokens.Punctuation(tokens.Underscore), xs)
          val u = Ident(Names.WILDCARD, posOfHead(xs))
          (Select(q, u, q.pos), r) 
        case (q, r) =>
          (q, r)
      }
      val rest3 = parseSemi(qual, rest2)
      (Import(qual, posOfHead(tokenList)), rest3)
    }

    private def parsePlugin(tokenList: TokenList): (PluginDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Plugin), tokenList)
      val (name, rest2) = parseId(rest1)
      val (args, rest3) = parseIDArgs(rest2)
      val (body, rest4) = parsePluginBody(rest3)
      val plgn = PluginDef(name, args, body, posOfHead(tokenList))
      val rest5 = parseOrReport(tokens.Punctuation(tokens.Semi), rest4)
      (plgn, rest5)
    }

    private def parsePhase(tokenList: TokenList): (PhaseDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Phase), tokenList)
      val (id, rest2) = parseId(rest1)
      val rest3 = parseOrReport(tokens.Punctuation(tokens.LParan), rest2)
      val (lit, rest4) = parseLiteral(rest3)
      val rest5 = parseOrReport(tokens.Punctuation(tokens.RParan), rest4)
      val (body, pre, perf, rest6) = parsePhaseBody(rest5) 
      val phse = PhaseDef(id, lit.valueAsString, pre, perf, 
                          body, posOfHead(tokenList))
      val rest7 = parseOrReport(tokens.Punctuation(tokens.Semi), rest6)
      (phse, rest7)
    }


    private def parseDef(tokenList: TokenList): (List[DefDef], TokenList) = {
      // val (mod, rest1) = parseModifier(tokenList)
      val rest1 = parseOrReport(tokens.Keyword(tokens.Def), tokenList)
      rest1 match {
        case tokens.Punctuation(tokens.LParan) :: xs =>
          parseMultiDef(tokenList)
        case _ =>
          val (name, rest2) = parseId(rest1)
          val (tparams, rest3) = parseGenerics(rest2)
          val (params, rest4) = parseParams(rest3)
          val mod = params match {
            case Nil => Modifier(Modifier.VARIABLE)
            case _ => Modifier(Modifier.DEF)
          }
          val rest5 = parseOrReport(tokens.Punctuation(tokens.Colon), rest4)
          val (tpe, rest6) = parseType(rest5)
          val rest7 = parseOrReport(tokens.Punctuation(tokens.Assign), rest6)
          val (rhs, rest8) = parseExpression(rest7)
          val rest9 = parseSemi(rhs, rest8)
          val defdef = DefDef(mod, name, tparams, params, tpe, 
            rhs, posOfHead(tokenList))
          (List(defdef), rest9)
      }
    }


    private def parseMultiDef(tokenList: TokenList): (List[DefDef], TokenList) = {
      
      @tailrec def toDefs(mod: Modifier, ids: List[DefDef], 
            rhs: Expression, acc: List[DefDef]): List[DefDef] = {
        ids match {
          case Nil => acc.reverse
          case x :: xs =>
            val selected = s"_${acc.size + 1}"
            val nme = Ident(selected, rhs.pos)
            val projector = Select(rhs, nme, rhs.pos)
            val v = DefDef(mod, x.name, Nil, Nil, x.tpe, projector, x.pos)
            toDefs(mod, xs, rhs, v :: acc)
        }
      }

      val mod = Modifier(Modifier.VARIABLE)
      val rest1 = parseOrReport(tokens.Keyword(tokens.Def), tokenList)
      val (defs, rest2) = parseParams(rest1)
      // defs match {
      //   case Nil =>
      //     reporter.report("(", ":", opsOfHead(rest3), BAD_TOKEN)
      //   case x :: Nil =>
      //     reporter.report("Pattern matching needs " + 
      //           "to be at least of length 2", opsOfHead(rest3))
      //   case _ => ()
      // }
      val rest3 = parseOrReport(tokens.Punctuation(tokens.Assign), rest2)
      val (rhs, rest4) = parseExpression(rest3)
      val rest5 = parseSemi(rhs, rest4)
      (toDefs(mod, defs, rhs, Nil), rest5)
    }

    private def parseId(tokenList: TokenList): (Ident, TokenList) = {
      tokenList match {
        case tokens.Id(x, pos) :: xs =>
          (Ident(x, pos), xs)
        case x :: xs =>
          // FIXME: Check if this error recovery sufficient
          val pos = posOfHead(tokenList)
          reporter.report("identifier", x.toString, pos, BAD_TOKEN) 
          (Ident(Names.uniqueName("noID"), pos), x :: xs)
      }
    }

    private def parseParams(tokenList: TokenList): (List[DefDef], TokenList) = {
      def parseParam(tokenss: TokenList): (DefDef, TokenList) = {
        val (id, rest1) = parseId(tokenss)
        val rest2 = parseOrReport(tokens.Punctuation(tokens.Colon), rest1)
        val (mod, rest3) = rest2 match {
          case tokens.Punctuation(tokens.Arrow) :: xs =>
            val m = Modifier(Modifier.PARAM | Modifier.BYNAME)
            (m, xs)
          case _ => (Modifier(Modifier.PARAM), rest2)
        }
        val (tpe, rest4) = parseType(rest3)
        val m = DefDef(mod, id, Nil, Nil, tpe, id, id.pos)
        (m, rest4)
      }
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      sequenceHelper[DefDef](rest1, Nil, parseParam, 
            tokens.Punctuation(tokens.RParan))
    }

    /**
     * TODO:
     * Make sure to convert the Type names to fully qualified names
     * Using the import statements
     */
    private def parseSimpleType(tokenss: TokenList): 
          (SimpleTypeTree, TokenList) = {
      def helperTArgs(tlist: TokenList): (List[TypeTree], TokenList) = {
        sequenceHelper[TypeTree](tokenss, Nil, parseType, 
            tokens.Punctuation(tokens.RBracket))
      }
      val (id, rest1) = parseSelectOrIdent(tokenss)
      val (targs, rest2) = rest1 match {
        case tokens.Punctuation(tokens.LBracket) :: xs =>
          helperTArgs(xs)
        case _ =>
        (Nil, rest1)
      }
      (SimpleTypeTree(id, targs, id.pos), rest2)
    }


    private def parseType(tokenList: TokenList): 
            (TypeTree, TokenList) = {
      def parseProductType(tokenss: TokenList): (List[TypeTree], TokenList) = {
        sequenceHelper[TypeTree](tokenss, Nil, parseType, 
              tokens.Punctuation(tokens.RParan))
      }
      
      
      tokenList match {
        case tokens.Punctuation(tokens.LParan) :: xs =>
          val (product, rest1) = parseProductType(xs)
          rest1 match {
            case tokens.Punctuation(tokens.Arrow) :: xs =>
              val (r, rest2) = parseType(xs)
              (FunctionTypeTree(product, r, posOfHead(tokenList)), rest2)
            case _ =>
              (ProductTypeTree(product, posOfHead(tokenList)), rest1)
          }
        case _ =>
          parseSimpleType(tokenList)
      }
    }


    private def parseNew(tokenList: TokenList): (New, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.New), tokenList)
      val (tpe, rest2) = parseSimpleType(rest1)
      val (args, rest3) = parseArgs(rest2)
      val newTree = New(tpe, args, posOfHead(tokenList))
      (newTree, rest3)
    }

    private def parseApply(tokenList: TokenList, qual: Expression): 
        (Apply, TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      val (targs, rest2) = parseTArgs(rest1)
      val (args, rest3) = parseArgs(rest2)
      val rest4 = parseOrReport(tokens.Punctuation(tokens.RParan), rest3)
      val apply = Apply(qual, targs, args, qual.pos)
      (apply, rest4)
    }

    private def parseThrow(tokenList: TokenList): (Throw, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Throw), tokenList)
      val (expr, rest2) = parseExpression(rest1)
      val throwTree = Throw(expr, expr.pos)
      (throwTree, rest2)
    }

    @tailrec private def sequenceHelper[T](tokenss: TokenList, acc: List[T], 
      f: (TokenList) => (T, TokenList), ender: tokens.Punctuation): 
          (List[T], TokenList) = {
      val (id, rest1) = f(tokenss)
      rest1 match {
        case tokens.Punctuation(tokens.Coma) :: xs =>
          sequenceHelper[T](xs, id :: acc, f, ender)
        case `ender` :: xs =>
          val ids = (id :: acc).reverse
          (ids, xs)
        case x :: xs =>
          val ids = (id :: acc).reverse
          reporter.report(x, ender, BAD_TOKEN)
          (ids, rest1)
      }
    }

    private def parseTArgs(tokenList: TokenList): (List[TypeTree], TokenList) = {
      
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LBracket), tokenList)
      sequenceHelper[TypeTree](rest1, Nil, parseType, 
        tokens.Punctuation(tokens.RBracket))
    }

    private def parseArgs(tokenList: TokenList): (List[Expression], TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      rest1 match {
        case tokens.Punctuation(tokens.RParan) :: xs =>
          (Nil, xs)
        case _ =>
          sequenceHelper[Expression](rest1, Nil, parseExpression, 
            tokens.Punctuation(tokens.RParan))
      }
    }

    private def parseIDArgs(tokenList: TokenList): 
        (List[SelectOrIdent], TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      sequenceHelper[SelectOrIdent](rest1, Nil, parseSelectOrIdent, 
            tokens.Punctuation(tokens.RParan))
    }

    private def parseGenerics(tokenList: TokenList): 
        (List[TParamDef], TokenList) = {
          
      def parseTParam(tokenss: TokenList): (TParamDef, TokenList) = {
        val (id, rest1) = parseId(tokenss)
        val (lbound, rest2) = rest1 match {
          case tokens.Punctuation(tokens.SuperType) :: xs =>
            parseType(xs)
          case _ =>
            val pos = posOfHead(rest1)
            (SimpleTypeTree(Ident("Nothing", pos), Nil, pos), rest1)
        }
        val (ubound, rest3) = rest2 match {
          case tokens.Punctuation(tokens.SubType) :: xs =>
            parseType(xs)
          case _ =>
            val pos = posOfHead(rest2)
            (SimpleTypeTree(Ident("Any", pos), Nil, pos), rest2)
        }

        val tparam = TParamDef(id, lbound, ubound, id.pos)
        (tparam, rest3)
      }

      tokenList match {
        case tokens.Punctuation(tokens.LBracket) :: xs =>
          sequenceHelper[TParamDef](xs, Nil, parseTParam, 
                tokens.Punctuation(tokens.RBracket))
        case _ => (Nil, tokenList)
      }
    }

    // private def parseModifier(tokenList: TokenList): (Modifier, TokenList) = {
    //   tokenList match {
    //     case tokens.Keyword(tokens.Private) :: xs =>
    //       (Modifier(Modifier.PRIVATE), xs)
    //     case xs => (Modifier(Modifier.PUBLIC), xs)
    //   }
    // }


    private def parseSemi(expr: Expression, 
            tokenList: TokenList): TokenList = {
      shouldBeFollowedBySemi(expr) match {
        case true =>
          parseOrReport(tokens.Punctuation(tokens.Semi), tokenList)
        case _ =>
          tokenList
      }
    }
    private def parseCheckOrTransform(tokenList: TokenList): 
        (DefDef, TokenList) = {
      val pos = posOfHead(tokenList)
      val (name, rest1) = tokenList match {
        case tokens.Keyword(tokens.Transform) :: xs => 
          (Ident(Names.TRANSFORMER, pos), xs)
        case tokens.Keyword(tokens.Check) :: xs => (Ident(Names.CHECKER, pos), xs)
        case xs =>
          reporter.report(Names.TRANSFORMER, pos, BAD_TOKEN)
          (Ident(Names.TRANSFORMER, pos), xs)
      }
      val rest2 = parseOrReport(tokens.Punctuation(tokens.Assign), rest1)
      val (rhs, rest3) = parseExpression(rest2)
      val rest4 = parseSemi(rhs, rest3)
      val tpe = SimpleTypeTree(Ident(Names.TREE_TYPE, pos), Nil, pos)

      val mod = {
        if(name.name == Names.TRANSFORMER) 
          Modifier(Modifier.TRANSFORMER) 
        else
          Modifier(Modifier.CHECKER) 
      }
      val m = DefDef(mod, name, Nil, Nil, tpe, rhs, pos)
      (m, rest4)
    }

    private def parsePreamble(tokenList: TokenList): (PropertyTree, TokenList) = {
      val pos = posOfHead(tokenList)
      val (lhs, rest1) = tokenList match {
        case tokens.Keyword(tokens.RunsBefore) :: xs => (RunsBeforeProperty, xs)
        case tokens.Keyword(tokens.RunsAfter) :: xs => (RunsAfterProperty, xs)
        case tokens.Keyword(tokens.RunsRightAfter) :: xs => 
              (RunsRightAfterProperty, xs)
        case xs =>
          (NoProperty, xs)
      }
      val (rhs, rest2) = parseArgs(rest1)
      val rhsPos = rhs match {
        case Nil => pos
        case x :: xs => x.pos
      }
      val assign = lhs match {
        case RunsRightAfterProperty =>
          PropertyTree(lhs, Apply(Ident("Some", rhsPos), Nil, rhs, rhsPos), pos)
        case _ =>
          PropertyTree(lhs, Apply(Ident("List", rhsPos), Nil, rhs, rhsPos), pos)
      }
      val rest3 = parseOrReport(tokens.Punctuation(tokens.Semi), rest2)
      (assign, rest3)
    }

    private def parsePluginBody(tokenList: TokenList): 
        (List[DefDef], TokenList) = {
      def helper(tokenss: TokenList): (List[DefDef], TokenList) = {
        tokenss match {
          case tokens.Keyword(tokens.Def) :: xs =>
            val (ms, r) = parseDef(tokenss)
            val (defs, rest) = helper(r)
            (ms ++ defs, rest)
          case _ => (Nil, tokenss)
        }
      }
      tokenList match {
        case tokens.Punctuation(tokens.LCurly) :: xs =>
          val (defs, rest1) = helper(xs)
          val rest2 = parseOrReport(tokens.Punctuation(tokens.RCurly), rest1)
          (defs, rest2)
        case _ =>
          (Nil, tokenList)
      }
    }

    @tailrec private def seenPreamble(preambles: List[PropertyTree], 
        pre: PropertyTree): Boolean = {
      preambles match {
        case Nil => false
        case x :: xs if x.property == pre.property => true
        case x :: xs => seenPreamble(xs, pre)
      }
    }

    private def parsePhaseBody(tokenList: TokenList): 
        (List[Tree], List[PropertyTree], DefDef, TokenList) = {
      // TODO: Make this tailrec
      @tailrec def helper(tokenss: TokenList, body: List[Tree], 
          preambles: List[PropertyTree], performer: Option[DefDef]): 
              (List[Tree], List[PropertyTree], Option[DefDef], TokenList) = {
        tokenss match {
          case tokens.Punctuation(tokens.RCurly) :: xs =>
            (body, preambles, performer, xs)
          case (tokens.Keyword(tokens.RunsBefore) |
                tokens.Keyword(tokens.RunsAfter) |
                tokens.Keyword(tokens.RunsRightAfter)) :: xs =>
            val (pre, r) = parsePreamble(tokenss)
            if(seenPreamble(preambles, pre)) {
              helper(r, body, preambles, performer)
            } else {
              helper(r, body, pre :: preambles, performer)
            }
          case tokens.Keyword(tokens.Def) :: xs =>
            val (ms, r) = parseDef(tokenss)
            helper(r, ms ++ body, preambles, performer)
          case (tokens.Keyword(tokens.Transform) |
               tokens.Keyword(tokens.Check)) :: xs if performer == None => 
            val (p, r) = parseCheckOrTransform(tokenss)
            helper(r, body, preambles, Some(p))
          case x :: xs =>
            reporter.report(tokens.Punctuation(tokens.RCurly), x, BAD_TOKEN)
            (body, preambles, performer, tokenss)
          case _ =>
            reporter.report("}", posOfHead(tokenss), BAD_TOKEN)
            (body, preambles, performer, tokenss)
        }
      }
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LCurly), tokenList)
      val (body, preambles, actionOpt, rest) = helper(rest1, Nil, Nil, None)
      actionOpt match {
        case Some(x) => (body, preambles, x, rest)
        case None =>
          val pos = posOfHead(rest)
          val dummyName = Ident(Names.uniqueName(Names.DUMMY_NAME), pos)
          val dummyType = SimpleTypeTree(dummyName, Nil, dummyName.pos)
          val dummy = DefDef(Modifier(Modifier.VARIABLE), 
              dummyName, Nil, Nil, dummyType, dummyName, pos)
          reporter.report(Names.TRANSFORMER, "}", pos, BAD_TOKEN)
          (body, preambles, dummy, rest)
      }
    }


    @tailrec private def parseSelectOrIdent(tokenList: TokenList, 
        acc: List[Ident]): (List[Ident], TokenList) = {
      val (id, rest) =  parseId(tokenList)
      rest match {
        case tokens.Punctuation(tokens.Dot) :: 
          tokens.Punctuation(tokens.Underscore) :: xs =>
          ((id :: acc).reverse, rest)
        case tokens.Punctuation(tokens.Dot) :: xs =>
          parseSelectOrIdent(xs, id :: acc)
        case _ => 
          ((id :: acc).reverse, rest)
      }
    }


    @tailrec private def toSelect(expr: Expression, ids: List[Ident]): Select = {
      ids match {
        case Nil =>
          // This should never happen
          val dummyName = Ident(Names.uniqueName(Names.DUMMY_NAME), expr.pos)
          Select(expr, dummyName, expr.pos)
        case x :: Nil =>
          Select(expr, x, expr.pos)
        case x :: y :: xs =>
          val select = Select(expr, x, expr.pos)
          toSelect(select, y :: xs)
      }
    }


    private def parseSelectOrIdent(tokenList: TokenList):
      (SelectOrIdent, TokenList) = {
      val (ids, rest1) = parseSelectOrIdent(tokenList, Nil)
      ids match {
        case Nil =>
          // This should never happen
          val dummyName = Ident(Names.uniqueName(Names.DUMMY_NAME), 
                posOfHead(tokenList))
          (dummyName, rest1)
        case x :: Nil =>
          (x, rest1)
        case x :: xs => (toSelect(x, xs), rest1)
      }
    }

    private def parseLiteral(tokenList: TokenList): (Literal, TokenList) = {
      tokenList match {
        case tokens.Literal(v, pos) :: xs => (Literal(v, pos), xs)
        case xs =>
          val pos = posOfHead(xs)
          reporter.report("literal", pos, BAD_TOKEN)
          (Literal("", pos), xs)
      }
    }
    
    private def parseIf(tokenList: TokenList): (If, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.If), tokenList)
      val rest2 = parseOrReport(tokens.Punctuation(tokens.LParan), rest1)
      val (cond, rest3) = parseExpression(rest2)
      val rest4 = parseOrReport(tokens.Punctuation(tokens.RParan), rest3)
      val (thenp, rest5) = parseExpression(rest4)
      val rest6 = parseOrReport(tokens.Keyword(tokens.Else), rest5)
      val (elsep, rest7) = parseExpression(rest6)
      (If(cond, thenp, elsep, posOfHead(tokenList)), rest7)
    }


    private def parseCase(tokenList: TokenList): (CaseDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Case), tokenList)
      val (pattern, rest2) = parsePattern(rest1)
      val (cond, rest3) = rest2 match {
        case tokens.Keyword(tokens.If) :: 
            tokens.Punctuation(tokens.LCurly) :: xs =>
          val (e, r) = parseExpression(xs)
          val r1 = parseOrReport(tokens.Punctuation(tokens.RCurly), r)
          (Some(e), r1)
        case tokens.Keyword(tokens.If) :: xs =>
          val (e, r) = parseExpression(xs)
          (Some(e), r)
        case _ =>
          (None, rest2)
      }
      val (rhs, rest4) = rest3 match {
        case tokens.Punctuation(tokens.LCurly) :: xs =>
          parseBlock(rest3)
        case _ => parseBlockLike(rest3, tokens.Keyword(tokens.Case))
      }


      val cdef = CaseDef(pattern, cond, rhs, posOfHead(tokenList))
      (cdef, rest3)
    }

    private def parsePattern(tokenList: TokenList): (Pattern, TokenList) = {
      def helper(tokenss: TokenList, id: Ident, 
        should: Boolean): (Pattern, TokenList) = {
        val (qual, rest1) = parseSelectOrIdent(tokenss)
        val (ps, rest2) = rest1 match {
          case tokens.Punctuation(tokens.LParan) :: xs => 
            sequenceHelper[Pattern](xs, Nil, parsePattern, 
              tokens.Punctuation(tokens.RParan))
          case _ if !should => (Nil, rest1)
          case x :: xs =>
            reporter.report(tokens.Punctuation(tokens.LParan), x, BAD_TOKEN)
            (Nil, rest1)
          case _ =>
            reporter.report("(", EOF, qual.pos, BAD_TOKEN)
            (Nil, rest1)
        }
        (Bind(id, qual, ps, id.pos), rest2)
      }
      tokenList match {
        case (x: tokens.Literal) :: xs => 
          val (lit, rest1) = parseLiteral(tokenList)
          (LiteralPattern(lit, lit.pos), rest1)
        case tokens.Id(_, _) :: tokens.Punctuation(tokens.At) :: xs =>
          val (id, rest1) = parseId(tokenList)
          val rest2 = parseOrReport(tokens.Punctuation(tokens.At), rest1)
          helper(rest2, id, true)
        case tokens.Id(_, pos) :: tokens.Punctuation(tokens.Dot) :: xs => 
          val star = Ident(Names.WILDCARD, pos)
          helper(tokenList, star, false)
        case tokens.Id(_, _) :: tokens.Punctuation(tokens.Colon) :: xs =>
          val (id, rest1) = parseId(tokenList)
          val (tp, rest2) = parseSelectOrIdent(rest1)
          (Bind(id, tp, Nil, id.pos), rest2)
        case tokens.Id(_, _) :: xs =>
          val (id, rest) = parseId(tokenList)
          val tp = Ident("Any", id.pos)
          (Bind(id, tp, Nil, id.pos), rest)
      }
    }

    private def parseCases(tokenList: TokenList): (List[CaseDef], TokenList) = {
      @tailrec def helper(tokenss: TokenList, acc: List[CaseDef]): 
        (List[CaseDef], TokenList) = {
        tokenss match {
          case tokens.Punctuation(tokens.RCurly) :: xs =>
            (acc.reverse, xs)
          case xs =>
            val (c, rest1) = parseCase(xs)
            helper(rest1, c :: acc)
        }
      }
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LCurly), tokenList) 
      helper(rest1, Nil)
    }

    private def parseMatch(expr: Expression, tokenList: TokenList): 
        (Match, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Match), tokenList) 
      val (cases, rest2) = parseCases(rest1)
      (Match(expr, cases, expr.pos), rest2)
    }

    private def parseTry(tokenList: TokenList): (Try, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Try), tokenList)
      val (expr, rest2) = parseExpression(rest1)
      val rest3 = parseOrReport(tokens.Keyword(tokens.Catch), rest2)
      val (cases, rest4) = parseCases(rest3)
      val (fnly, rest5) = rest4 match {
        case tokens.Keyword(tokens.Finally) :: xs =>
          val (expr, rest) = parseExpression(xs)
          (Some(expr), rest)
        case _ =>
          (None, rest4)
      }
      (Try(expr, cases, fnly, expr.pos), rest5)
    }
    private def shouldBeFollowedBySemi(expr: Expression): Boolean = {
      expr match {
        // case x: Block => false
        // case x: Match => false
        // case x: If => false
        // case x: Try => false
        case _ => true
      }
    }
    private def parseBlockLike(tokenList: TokenList,
      end: tokens.Token = tokens.Punctuation(tokens.RCurly)): 
        (Block, TokenList) = {
      
      @tailrec def helper(tokenss: TokenList, acc: List[PositionedTree]): 
          (Block, TokenList) = {
        val (stmts, rest1) = tokenss match {
          case tokens.Keyword(tokens.Def) :: xs => 
            parseDef(tokenss)
          case _ =>
            val (expr, rest1) = parseExpression(tokenss)
            val rest2= parseSemi(expr, rest1)
            (List(expr), rest2)
        }
        rest1 match {
          case tokens.Punctuation(`end`) :: xs =>
            val contents = acc ++ stmts
            val block = contents.splitAt(contents.size) match {
              case (ss, List(e: Expression)) =>
                Block(ss, e, posOfHead(tokenss))
              case _ =>
                Block(contents, Literal((), posOfHead(rest1)), posOfHead(tokenss))
            }
            (block, xs)
          case _ => helper(rest1, acc ++ stmts)
        }
      }
      helper(tokenList, Nil)
    }

    private def parseBlock(tokenList: TokenList): (Block, TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LCurly), tokenList)
      parseBlockLike(rest1)
    }


    
    private def parseOrReport(expected: tokens.Token, 
      tokenList: TokenList): TokenList = {
      tokenList match {
        case `expected` :: rest => rest
        case x :: rest =>
          reporter.report(expected, x, BAD_TOKEN)
          // FIXME: 
          // We simply return the whole list here, is it a correct
          // error recovery appraoch?
          tokenList
        case Nil =>
          reporter.report(expected, tokens.EmptyToken, BAD_TOKEN) 
          Nil
      }
    }

    

    private def parseUnary(tokenList: TokenList, 
        pos: Position, uop: UniOp): (Unary, TokenList) = {
      val (expr, rest1) = parseExpression(tokenList)
      (Unary(uop, expr, pos), rest1)
    }


    private def parseBinary(tokenList: TokenList, lhs: Expression, bop: BinOp): 
        (Binary, TokenList) = {
      val (rhs, rest1) = parseExpression(tokenList)
      (Binary(lhs, bop, rhs, lhs.pos), rest1)
    }
    

    private def parseFun(tokenList: TokenList): (Function, TokenList) = {
      val (params, rest1) = parseParams(tokenList)
      val rest2 = parseOrReport(tokens.Punctuation(tokens.Arrow), rest1)
      val (expr, rest3) = parseExpression(rest2)
      (Function(params, expr, posOfHead(tokenList)), rest3)
    }

    private def parseSelectExprOrIdent(tokenList: TokenList, 
        expr: Expression): (SelectOrIdent, TokenList) = {
      val (ids, rest1) = parseSelectOrIdent(tokenList, Nil)
      val select = toSelect(expr, ids)
      (select, rest1)
    }


    @tailrec private def isFun(tokenList: TokenList, count: Int = 1): Boolean = {
      tokenList match {
        case tokens.Punctuation(tokens.Arrow) :: xs if count == 0 => true
        case tokens.Punctuation(tokens.LParan) :: xs => isFun(xs, count + 1)
        case tokens.Punctuation(tokens.RParan) :: xs => isFun(xs, count - 1)
        case _ if count == 0 => false
        case x :: xs => isFun(xs, count)
        case Nil => false
      }
    }

    private def parseRecord(tokenList: TokenList): (Expression, TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      val (exprs, rest2) = sequenceHelper[Expression](rest1, Nil, 
          parseExpression, tokens.Punctuation(tokens.RParan))
      exprs match {
        case Nil =>
          (Literal((), posOfHead(tokenList)), rest2)
        case x :: Nil =>
          (x, rest2)
        case x :: xs =>
          val record = Record(exprs, x.pos)
          (record, rest2)
      }
    }

    /**
     * An expression is one of the following:
     * <ul>
     *  <li> Ident
     *  <li> Select
     *  <li> Match
     *  <li> If
     *  <li> Fun
     *  <li> Apply
     *  <li> Literal
     *  <li> Unary Operation
     *  <li> Binary Operation
     *  <li> Block
     *  <li> Try
     *  <li> (Expression)
     *  <li> Throw
     *  <li> New
     * </ul>
     */
    // TODO: Turn this into a tail recursive function
    private def parseExpression(tokenList: TokenList): (Expression, TokenList) = {
      val (parsed, rest1) = tokenList match {
        case tokens.Punctuation(tokens.Underscore) :: xs =>
          // Not sure if we need this at this moment or not
          (Ident(Names.WILDCARD, posOfHead(tokenList)), xs)
        case tokens.Punctuation(tokens.Minus) :: xs =>
          parseUnary(xs, posOfHead(tokenList), Negative)
        case tokens.Keyword(tokens.This) :: xs =>
          (This(posOfHead(tokenList)), xs)
        case tokens.Keyword(tokens.Super) :: xs =>
          (Super(posOfHead(tokenList)), xs)
        case tokens.Punctuation(tokens.Not) :: xs =>
          parseUnary(xs, posOfHead(tokenList), Not)
        case tokens.Keyword(tokens.If) :: xs =>
          parseIf(tokenList)
        case tokens.Keyword(tokens.Try) :: xs =>
          parseTry(tokenList)
        case tokens.Punctuation(tokens.LParan) :: xs if isFun(xs) =>
          parseFun(tokenList)
        case tokens.Punctuation(tokens.LParan) :: xs =>
          parseRecord(tokenList)
        case tokens.Punctuation(tokens.LCurly) :: xs =>
          parseBlock(tokenList)
        case tokens.Keyword(tokens.Throw) :: xs =>
          parseThrow(tokenList)
        case tokens.Keyword(tokens.New) :: xs =>
          parseNew(tokenList)
        case tokens.Literal(_, _) :: xs =>
          parseLiteral(tokenList)
        case _ => parseSelectOrIdent(tokenList)
      }
      parseRestOfExpr(parsed, rest1)
    }


    private def parseRestOfExpr(expr: Expression, 
          tokenList: TokenList): (Expression, TokenList) = {
      def isAppliable(e: Expression): Boolean = {
        e match {
          case x: If => false
          case x: Try => false
          case x: Function => false
          case x: Record => false
          case x: Block => false
          case x: Throw => false
          case _ => true
        }
      }
      def isTypeAppliable(e: Expression): Boolean = {
        e match {
          case x: Unary => false
          case _ => isAppliable(e)
        }
      }
      def isBinary(p: tokens.Punctuations): Boolean = {
        p match {
          case tokens.Plus |
              tokens.Minus |
              tokens.Div |
              tokens.Mul |
              tokens.Mod |
              tokens.Le |
              tokens.Ge |
              tokens.Eq |
              tokens.Ne |
              tokens.LT |
              tokens.GT |
              tokens.Xor |
              tokens.And |
              tokens.Or |
              tokens.Cons|
              tokens.Join |
              tokens.To |
              tokens.SHR |
              tokens.SHL => true
          case _ => false
        }
      }

      def toBinOp(p: tokens.Punctuations): BinOp = {
        p match {
          case tokens.Plus => Add
          case tokens.Minus => Sub
          case tokens.Div => Div
          case tokens.Mul => Mul
          case tokens.Mod => Mod
          case tokens.Le => LE
          case tokens.Ge => GE
          case tokens.Eq => Eq
          case tokens.Ne => Neq
          case tokens.LT => LT
          case tokens.GT => GT
          case tokens.Xor => XOR
          case tokens.And => And
          case tokens.Or => Or
          case tokens.SHR => SHR
          case tokens.SHL => SHL
          case tokens.Cons => Cons
          case tokens.Join => Join
          case tokens.To => To
          case _ => 
            // This should not happen
            Add
        }
      }
      tokenList match {
        case tokens.Keyword(tokens.Match) :: xs =>
          parseMatch(expr, tokenList)
        case tokens.Punctuation(tokens.Dot) :: xs => 
          isAppliable(expr) match {
            case true => parseSelectExprOrIdent(tokenList, expr)
            case false => (expr, tokenList)
          } 
        case tokens.Punctuation(tokens.LParan) :: xs => 
          isAppliable(expr) match {
            case true => parseApply(tokenList, expr)
            case false => (expr, tokenList)
          } 
        case tokens.Punctuation(tokens.LBracket) :: xs => 
          isTypeAppliable(expr) match {
            case true => parseApply(tokenList, expr)
            case _ => (expr, tokenList)
          }
        case tokens.Punctuation(x) :: xs 
            if isBinary(x) && isTypeAppliable(expr) =>
          parseBinary(xs, expr, toBinOp(x))
        case _ => (expr, tokenList)
      }
    }


    private def posOfHead(tokens: TokenList): Position = {
      val dummy = Position()
      tokens match {
        case Nil => dummy
        case x :: xs => x.position.getOrElse(dummy)
      }
    }
  }


  // TODO: Eliminate this pass, and burry it in Lexer
  class Normalizer extends Phase {
    type InputType = List[TokenList]
    type OutputType = List[TokenList]
    val name = "normalizer"

    val runsAfter: Option[String] = Some("lexer")

    def run(tokenList: InputType): OutputType = {
      val noEmpty = tokenList.map((x) => {
        x.filter((y) => y != tokens.EmptyToken)
      })
      val removedExtraNL = noEmpty.map(removeExtra(_, 
          tokens.Punctuation(tokens.NL)))
      val f = removedExtraNL.map(normalize(_))
      val r = f.map(removeExtra(_, tokens.Punctuation(tokens.Semi)))
      // r.foreach((x) => x.map(println(_)))
      r.map((x) => {
        x.filter((y) => y != tokens.Punctuation(tokens.NL))
      })
    }


    @tailrec private def removeExtra(tokenList: TokenList, tkn: tokens.Token,
            collected: TokenList = Nil): TokenList = {
      tokenList match {
        case Nil => 
          collected.reverse
        case `tkn` :: `tkn` :: xs => 
          removeExtra(tkn :: xs, tkn, collected)
        case x :: xs => removeExtra(xs, tkn, x :: collected)
      }
    }

    @tailrec private def normalize(tokenList: TokenList,
          collected: TokenList = Nil): TokenList = {
      tokenList match {
        case x :: Nil => 
          normalize(Nil, tokens.Punctuation(tokens.Semi) :: x :: collected)
        case Nil => 
          collected.reverse
        case x :: tokens.Punctuation(tokens.NL) :: y :: xs 
            if canEnd(x) && canStart(y) =>
          normalize(y :: xs, tokens.Punctuation(tokens.Semi) :: x :: collected)
        case x :: tokens.Punctuation(tokens.NL) :: y :: xs =>
          normalize(y :: xs, x :: collected)
        case tokens.Punctuation(tokens.NL) :: xs => normalize(xs, collected)
        case x :: xs =>
          normalize(xs, x :: collected)
      }
    }


    private def canEnd(token: tokens.Token): Boolean = {
      token match {
        case tokens.Keyword(key) =>
          key match {
            case tokens.Tree | tokens.This => true
            case _ => false
          }
        case tokens.Punctuation(punc) =>
          punc match {
            case tokens.RCurly | tokens.RParan | tokens.RBracket |
                tokens.Underscore =>
              true
            case _ => false
          }
        case _ => true
      }
    }

    private def canStart(token: tokens.Token): Boolean = {
      token match {
        case tokens.Keyword(key) =>
          key match {
            case tokens.RunsAfter | tokens.RunsRightAfter |
                tokens.RunsBefore | tokens.Import | tokens.If |
                tokens.Package | tokens.Plugin | tokens.New | 
                tokens.Phase | tokens.Transform | tokens.Check | 
                tokens.Def | tokens.Super |
                tokens.This | tokens.Throw => true
            case _ => false
          }
        case tokens.Punctuation(punc) =>
          punc match {
            case tokens.RCurly | tokens.LCurly | tokens.LParan | tokens.Minus |
                tokens.Not | tokens.At =>
              true
            case _ => false
          }
        case _ => true
      }
    }

  }

  class Lexer extends Phase {
    type InputType = List[SourceFile]
    type OutputType = List[TokenList]
    val name: String = "lexer"

    val runsAfter: Option[String] = None

    def run(files: InputType): OutputType = {
      files.map(lexify(_))
    } 



    private def lexify(file: SourceFile): TokenList = {
      val chars = file.content
      lexify(chars)(file, 1)
    }

    


    // TODO: Make this tail recursive
    private def lexify(chars: List[Char], col: Int = 1, 
        read: String = "")(implicit file: SourceFile, row: Int): TokenList = {
      chars match {
        case Nil => Nil
        case x :: y :: xs if isComposedSymbol(x, y) =>
          val pos = Position(file, col - read.length, row)
          val posSym = pos.copy(col = col)
          identify(read, pos) :: identify(x, y, posSym) :: lexify(xs, col + 2)
        case '`' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posStr = pos.copy(col = col)
          val (rest, id, nrow, ncol) = 
            readVariable(xs, "", col + 1, row)(pos)
          identify(read, pos) :: id :: lexify(rest, ncol, "")(file, nrow)
        case '\'' :: '\\' :: y :: x :: '\'' :: xs if isEscape(y) =>
          val pos = Position(file, col - read.length, row)
          val posChar = pos.copy(col = col)
          identify(read, pos) :: tokens.Literal(x, posChar) :: 
              lexify(xs, col + 3)
        case '\'' :: x :: '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posChar = pos.copy(col = col)
          identify(read, pos) :: tokens.Literal(x, posChar) :: 
              lexify(xs, col + 3)
        case '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          reporter.report("'", pos, BAD_TOKEN)
          identify(read, pos) :: lexify(xs, col + 1)
        case '\"' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posStr = pos.copy(col = col)
          val (rest, strLit, ncol) = 
            readStringLiteral(xs, "", col + 1)(pos)
          identify(read, pos) :: strLit :: lexify(rest, ncol, "")
        case x :: xs if isIntegral(read + x) =>
          lexify(xs, col + 1, read + x)(file, row)
        case '.' :: xs if isIntegral(read) =>
          lexify(xs, col + 1, read + '.')(file, row)
        case ('e' | 'E') :: '-' :: xs if isDecimal(read) =>
          lexify(xs, col + 2, read + "e-")(file, row)
        case ('e' | 'E') :: '+' :: xs if isDecimal(read) =>
          lexify(xs, col + 2, read + "e+")(file, row)
        case ('e' | 'E') :: xs if isDecimal(read) =>
          lexify(xs, col + 1, read + "e")(file, row)
        case x :: xs if isDecimal(read + x) =>
          lexify(xs, col + 1, read + x)(file, row)
        case xs if isIntegral(read) =>
          val pos = Position(file, col - read.length, row)
          tokens.Literal(read.toInt, pos) :: lexify(xs, col, "")(file, row)
        case xs if isDecimal(read) =>
          val pos = Position(file, col - read.length, row)
          tokens.Literal(read.toDouble, pos) :: lexify(xs, col, "")(file, row)
        case '\n' :: xs => 
          val pos = Position(file, col - read.length, row)
          val posNL = pos.copy(col = col)
          identify(read, pos) :: tokens.Punctuation(tokens.NL, posNL) :: 
              lexify(xs)(file, row + 1)
        case '/' :: '*' :: xs =>
          val pos = Position(file, col, row)
          val prevPos = pos.copy(col = col - read.length)
          val (rest, block, ncol, nrow) = 
              readCommentBlock(xs, "", col + 2, row)(pos)
          // We do throw away comments now and do not persist them
          // identify(read, prevPos) :: block :: lexify(rest, ncol, "")(file, nrow)
          identify(read, prevPos) :: lexify(rest, ncol, "")(file, nrow)
        // case '.' :: '.' :: '/' :: xs =>
        //   val pos = Position(file, col, row)
        //   val prevPos = pos.copy(col = col - read.length)
        //   val (rest, block, ncol, nrow) = 
        //       readScalaBlock(xs, "", col + 3, row)(pos)
        // identify(read, prevPos) :: block :: lexify(rest, ncol, "")(file, nrow)
        case '/' :: '/' :: xs =>
          val pos = Position(file, col, row)
          val prevPos = pos.copy(col = col - read.length)
          // We do throw away comments and do not persist them now
          // identify(read, pos) :: List(tokens.CommentLine(xs.mkString, pos)) 
          val (rest, comment) = readComment(xs, "")(pos)
          identify(read, pos) :: lexify(rest, 1, "")(file, row + 1)
        case x :: xs if isSeparator(x) =>
          val xPos = Position(file, col, row)
          val readPos = xPos.copy(col = col - read.length)
          identify(read, readPos) :: identify(x, xPos) :: lexify(xs, col + 1)
        case x :: xs => 
          lexify(xs, col + 1, read + x)
      } 
    }


    // private def readScalaBlock(chars: List[Char], read: String, col: Int,
    //   row: Int)(implicit pos: Position): (List[Char], 
    //     tokens.ScalaBlock, Int, Int) = {
    //   chars match {
    //     case Nil => (Nil, tokens.ScalaBlock(read, pos), col, row)
    //     case '/' :: '.' :: '.' :: xs =>
    //       (xs, tokens.ScalaBlock(read, pos), col + 3, row)
    //     case ('\n' | '\r') :: xs =>
    //       readScalaBlock(xs, read, 1, row + 1)
    //     case x :: xs =>
    //       readScalaBlock(xs, read + x, col + 1, row)
    //   }
    // }


    @tailrec private def readVariable(chars: List[Char], read: String,
        col: Int, row: Int)(implicit pos: Position): 
        (List[Char], tokens.Id, Int, Int) = {
      chars match {
        case Nil =>
          reporter.report("`", LINE_FEED, pos.copy(col = col), BAD_TOKEN)
          (Nil, tokens.Id(read, pos), col, row)
        case '\n' :: xs =>
          reporter.report("`", LINE_FEED, pos.copy(col = col + 1), BAD_TOKEN)
          (xs, tokens.Id(read, pos), 1, row + 1)
        case '`' :: xs =>
          (xs, tokens.Id(read, pos), col + 1, row)
        case x :: xs =>
          readVariable(xs, read + x, col + 1, row)
      }

    }
    @tailrec private def readStringLiteral(chars: List[Char], read: String, 
      col: Int)(implicit pos: Position): 
          (List[Char], tokens.Literal, Int) = {
      chars match {
        case Nil =>
          reporter.report("\"", LINE_FEED, pos.copy(col = col), BAD_TOKEN)
          (Nil, tokens.Literal(read, pos), col)
        case '\\' :: y :: xs if isEscape(y) =>
          readStringLiteral(xs, read + "\\" + y, col + 2)
        case '\n' :: xs =>
          reporter.report("\"", LINE_FEED, pos.copy(col = col + 1), BAD_TOKEN)
          (xs, tokens.Literal(read, pos), 1)
        case '"' :: xs =>
          (xs, tokens.Literal(read, pos), col + 1)
        case x :: xs =>
          readStringLiteral(xs, read + x, col + 1)
      }
    }

    private def isEscape(x: Char): Boolean = {
      x match {
        case '\b' => true
        case '\t' => true
        case '\n' => true
        case '\f' => true
        case '\r' => true
        case '\"' => true
        case '\'' => true
        case '\\' => true
        case _ => false
      }
    }

    @tailrec private def readComment(chars: List[Char], read: String)
          (implicit pos: Position): 
        (List[Char], tokens.CommentLine) = {
      chars match {
        case Nil => (Nil, tokens.CommentLine(read, pos))
        case ('\n' | '\r') :: xs =>
          (xs, tokens.CommentLine(read, pos))
        case x :: xs =>
          readComment(xs, read + x)
      }
    }
    @tailrec private def readCommentBlock(chars: List[Char], read: String, 
      col: Int, row: Int)(implicit pos: Position): 
        (List[Char], tokens.CommentBlock, Int, Int) = {
      chars match {
        case Nil => (Nil, tokens.CommentBlock(read, pos), col, row)
        case '*' :: '/' :: xs =>
          (xs, tokens.CommentBlock(read, pos), col + 2, row)
        case '\n' :: xs =>
          readCommentBlock(xs, read, 1, row + 1)
        case x :: xs =>
          readCommentBlock(xs, read + x, col + 1, row)
      }
    }

    private def isSeparator(x: Char): Boolean = {
      // A list of the punctuations that can separate a word
      val separators = " \n{}()[]=+-/*%<>!|&`'\"_.;:\\,@"
      separators.contains(x) 
    }


    private def isIntegral(str: String): Boolean = {
      str match {
        case "" => false
        case s => 
          try {
            s.toInt
            true
          } catch{
            case e: NumberFormatException => false
          }
      }
    }

    private def isDecimal(str: String): Boolean = {
      str match {
        case "" => false
        case xs =>
          try { 
            xs.toDouble
            true
          } catch { 
            case e: NumberFormatException => false
          }
      }
    }


    private def isComposedSymbol(c1: Char, c2: Char): Boolean = {
      (c1, c2) match {
        case ('=', '=') => true
        case ('!', '=') => true
        case ('<', '=') => true
        case ('>', '=') => true
        case ('|', '|') => true
        case ('&', '&') => true
        case ('<', ':') => true
        case (':', '>') => true
        case _ => false
      }
    }

    private def identify(c1: Char, c2: Char, pos: Position): tokens.Token = {
      (c1, c2) match {
        case ('=', '=') => tokens.Punctuation(tokens.Eq, pos)
        case ('!', '=') => tokens.Punctuation(tokens.Ne, pos)
        case ('<', '=') => tokens.Punctuation(tokens.Le, pos)
        case ('>', '=') => tokens.Punctuation(tokens.Ge, pos)
        case ('|', '|') => tokens.Punctuation(tokens.Or, pos)
        case ('&', '&') => tokens.Punctuation(tokens.And, pos)
        case ('<', ':') => tokens.Punctuation(tokens.SubType, pos)
        case ('>', ':') => tokens.Punctuation(tokens.SuperType, pos)
        case ('=', '>') => tokens.Punctuation(tokens.Arrow, pos)
        case (':', ':') => tokens.Punctuation(tokens.Cons, pos)
        case ('+', '+') => tokens.Punctuation(tokens.Join, pos)
        case ('-', '>') => tokens.Punctuation(tokens.To, pos)
        case ('>', '>') => tokens.Punctuation(tokens.SHR, pos)
        case ('<', '<') => tokens.Punctuation(tokens.SHL, pos)
        case _ => tokens.EmptyToken
      }
    }

    private def identify(str: String, pos: Position): tokens.Token = {
      str match {
        case "runsAfter" => tokens.Keyword(tokens.RunsAfter, pos)
        case "runsRightAfter" => tokens.Keyword(tokens.RunsRightAfter, pos)
        case "runsBefore" => tokens.Keyword(tokens.RunsBefore, pos)
        case "import" => tokens.Keyword(tokens.Import, pos)
        case "if" => tokens.Keyword(tokens.If, pos)
        case "else" => tokens.Keyword(tokens.Else, pos)
        case "match" => tokens.Keyword(tokens.Match, pos)
        case "package" => tokens.Keyword(tokens.Package, pos)
        case "plugin" => tokens.Keyword(tokens.Plugin, pos)
        case "phase" => tokens.Keyword(tokens.Phase, pos)
        case "transform" => tokens.Keyword(tokens.Transform, pos)
        case "check" => tokens.Keyword(tokens.Check, pos)
        case "def" => tokens.Keyword(tokens.Def, pos)
        case "case" => tokens.Keyword(tokens.Case, pos)
        case "tree" => tokens.Keyword(tokens.Tree, pos)
        // case "private" => tokens.Keyword(tokens.Private, pos)
        case "this" => tokens.Keyword(tokens.This, pos)
        case "super" => tokens.Keyword(tokens.Super, pos)
        case "throw" => tokens.Keyword(tokens.Throw, pos)
        case "try" => tokens.Keyword(tokens.Try, pos)
        case "catch" => tokens.Keyword(tokens.Catch, pos)
        case "finally" => tokens.Keyword(tokens.Finally, pos)
        case "new" => tokens.Keyword(tokens.New, pos)
        case "true" => tokens.Literal(true, pos)
        case "false" => tokens.Literal(false, pos)
        case x if Names.isScala(x) =>
          reporter.report(x, pos, SCALA_KEYWORD)
          tokens.EmptyToken
        case "" => tokens.EmptyToken
        case _ => tokens.Id(str, pos)
      }
    }

    private def identify(char: Char, pos: Position): tokens.Token = {
      char match {
        case '^' => tokens.Punctuation(tokens.Xor, pos)
        case '{' => tokens.Punctuation(tokens.LCurly, pos)
        case '}' => tokens.Punctuation(tokens.RCurly, pos)
        case '[' => tokens.Punctuation(tokens.LBracket, pos)
        case ']' => tokens.Punctuation(tokens.RBracket, pos)
        case '(' => tokens.Punctuation(tokens.LParan, pos)
        case ')' => tokens.Punctuation(tokens.RParan, pos)
        case '=' => tokens.Punctuation(tokens.Assign, pos)
        case '+' => tokens.Punctuation(tokens.Plus, pos)
        case '-' => tokens.Punctuation(tokens.Minus, pos)
        case '/' => tokens.Punctuation(tokens.Div, pos)
        case '*' => tokens.Punctuation(tokens.Mul, pos)
        case '%' => tokens.Punctuation(tokens.Mod, pos)
        case '<' => tokens.Punctuation(tokens.LT, pos)
        case '>' => tokens.Punctuation(tokens.GT, pos)
        case '!' => tokens.Punctuation(tokens.Not, pos)
        case '|' => tokens.Punctuation(tokens.Pipe, pos)
        case '_' => tokens.Punctuation(tokens.Underscore, pos)
        case '.' => tokens.Punctuation(tokens.Dot, pos)
        case ';' => tokens.Punctuation(tokens.Semi, pos)
        case ':' => tokens.Punctuation(tokens.Colon, pos)
        // case '\\' => tokens.Punctuation(tokens.BackSlash, pos)
        case ',' => tokens.Punctuation(tokens.Coma, pos)
        case '@' => tokens.Punctuation(tokens.At, pos)
        case ' ' => tokens.EmptyToken
      }
    }

  }

  trait Tokens {

    sealed abstract class Punctuations
    // brackets, braces and curly brackets
    case object LCurly extends Punctuations {
      override def toString: String = "{"
    }
    case object RCurly extends Punctuations {
      override def toString: String = "}"
    }
    case object LParan extends Punctuations {
      override def toString: String = "("
    }
    case object RParan extends Punctuations {
      override def toString: String = ")"
    }
    case object LBracket extends Punctuations {
      override def toString: String = "["
    }
    case object RBracket extends Punctuations {
      override def toString: String = "]"
    }


    // mathmatical symbols
    case object Assign extends Punctuations {
      override def toString: String = "="
    }
    case object Plus extends Punctuations {
      override def toString: String = "+"
    }
    case object Minus extends Punctuations {
      override def toString: String = "-"
    }
    case object Div extends Punctuations {
      override def toString: String = "/"
    }
    case object Mul extends Punctuations {
      override def toString: String = "*"
    }
    case object Mod extends Punctuations {
      override def toString: String = "%"
    }

    // logical symbols
    case object LT extends Punctuations {
      override def toString: String = "<"
    }
    case object GT extends Punctuations {
      override def toString: String = ">"
    }
    case object Xor extends Punctuations {
      override def toString: String = "^"
    }
    case object Not extends Punctuations {
      override def toString: String = "!"
    }
    case object SHL extends Punctuations {
      override def toString: String = "<<"
    }
    case object SHR extends Punctuations {
      override def toString: String = ">>"
    }


    // composed symbols
    case object Le extends Punctuations {
      override def toString: String = "<="
    }
    case object Ge extends Punctuations {
      override def toString: String = ">="
    }
    case object Eq extends Punctuations {
      override def toString: String = "=="
    }
    case object Ne extends Punctuations {
      override def toString: String = "!="
    }
    case object SubType extends Punctuations {
      override def toString: String = "<:"
    }
    case object SuperType extends Punctuations {
      override def toString: String = ">:"
    }
    case object Arrow extends Punctuations {
      override def toString: String = "=>"
    }
    case object And extends Punctuations {
      override def toString: String = "&&"
    }
    case object Or extends Punctuations {
      override def toString: String = "||"
    }


    // other symbols
    case object Underscore extends Punctuations {
      override def toString: String = "_"
    }
    case object Dot extends Punctuations {
      override def toString: String = "."
    }
    case object Semi extends Punctuations {
      override def toString: String = ";"
    }
    case object Colon extends Punctuations {
      override def toString: String = ":"
    }
    // case object BackSlash extends Punctuations
    case object Coma extends Punctuations {
      override def toString: String = ","
    }
    case object At extends Punctuations {
      override def toString: String = "@"
    }
    case object NL extends Punctuations {
      override def toString: String = "new line"
    }
    case object Pipe extends Punctuations {
      override def toString: String = "|"
    }

    // Collection operators
    case object Join extends Punctuations {
      override def toString: String = "++"
    }
    case object Cons extends Punctuations {
      override def toString: String = "::"
    }
    case object To extends Punctuations {
      override def toString: String = "->"
    }

    
    // Lombrello keywords
    sealed abstract class Keywords
    case object RunsAfter extends Keywords {
      override def toString: String = "runsAfter"
    }
    case object RunsRightAfter extends Keywords {
      override def toString: String = "runsRightAfter"
    }
    case object RunsBefore extends Keywords {
      override def toString: String = "runsBefore"
    }
    case object Import extends Keywords {
      override def toString: String = "import"
    }
    case object If extends Keywords {
      override def toString: String = "if"
    }
    case object Else extends Keywords {
      override def toString: String = "else"
    }
    case object Match extends Keywords {
      override def toString: String = "match"
    }
    case object Package extends Keywords {
      override def toString: String = "pacakge"
    }
    case object Plugin extends Keywords {
      override def toString: String = "plugin"
    }
    case object Phase extends Keywords {
      override def toString: String = "phase"
    }
    case object Transform extends Keywords {
      override def toString: String = "transform"
    }
    case object Check extends Keywords {
      override def toString: String = "check"
    }
    case object Def extends Keywords {
      override def toString: String = "def"
    }
    case object Case extends Keywords {
      override def toString: String = "case"
    }
    case object Tree extends Keywords    {
      override def toString: String = "tree"
    }
    // case object Private extends Keywords
    case object Super extends Keywords {
      override def toString: String = "super"
    }
    case object This extends Keywords {
      override def toString: String = "this"
    }
    case object Throw extends Keywords {
      override def toString: String = "throw"
    }
    case object Try extends Keywords {
      override def toString: String = "try"
    }
    case object Catch extends Keywords {
      override def toString: String = "catch"
    }
    case object Finally extends Keywords {
      override def toString: String = "finally"
    }
    case object New extends Keywords {
      override def toString: String = "new"
    }

    sealed abstract class Token {
      def position: Option[Position]
    }

    sealed abstract class PositionedToken extends Token {
      val pos: Position
      def position: Option[Position] = Some(pos)
    }
   // case class ScalaBlock(verbatim: String, 
      // pos: Position) extends PositionedToken 
    case class CommentLine(verbatim: String, 
      pos: Position) extends PositionedToken
    case class CommentBlock(verbatim: String, 
      pos: Position) extends PositionedToken
    case class Id(name: String, pos: Position) extends PositionedToken
    case class Literal(value: Any, pos: Position) extends PositionedToken

    class Keyword(val keyword: Keywords, val pos: Position) 
      extends PositionedToken {

      override def toString: String = keyword.toString

      override def hashCode: Int = keyword.hashCode
      override def equals(that: Any): Boolean = {
        that match {
          case null => false
          case x: Keyword => x.keyword == keyword
          case _ => false
        }
      }
    }
    object Keyword {
      def unapply(keyword: Keyword): Option[Keywords] = {
        Some(keyword.keyword)
      }
      def apply(kind: Keywords): Keyword = {
        new Keyword(kind, Position())
      }
      def apply(keyword: Keywords, pos: Position): Keyword = {
        new Keyword(keyword, pos)
      }
    }
    class Punctuation(val kind: Punctuations, 
      val pos: Position) extends PositionedToken {

      override def toString: String = kind.toString

      override def hashCode: Int = kind.hashCode
      override def equals(that: Any): Boolean = {
        that match {
          case null => false
          case x: Punctuation => x.kind == kind
          case _ => false
        }
      }
    }
    object Punctuation {
      def unapply(punc: Punctuation): Option[Punctuations] = {
        Some(punc.kind)
      }
      def apply(kind: Punctuations): Punctuation = {
        new Punctuation(kind, Position())
      }
      def apply(kind: Punctuations, pos: Position): Punctuation = {
        new Punctuation(kind, pos)
      }
    }
    case object EmptyToken extends Token {
      def position = None
    }
  }
}
