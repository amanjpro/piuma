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
      // val trees = tokenss.map(parse(_))
      // Program(trees)
      null
    }

    private def parse(tokenList: TokenList): (Tree, TokenList) = {
      tokenList match {
        // case tokens.ScalaBlock(verbatim, pos) :: xs => 
          // (ScalaBlock(verbatim, pos), xs)
        case tokens.CommentBlock(verbatim, pos) :: xs =>
          (Comment(BlockComment, verbatim, pos), xs)
        case tokens.CommentLine(verbatim, pos) :: xs =>
          (Comment(LineComment, verbatim, pos), xs)
        case tokens.Keyword(tokens.Package) :: xs =>
          (parsePackage(tokenList), Nil)
        case Nil => (NoTree, Nil)
        case xs =>
          val pos = posOfHead(xs)
          val pid = Ident(Names.EMPTY_PACKAGE, pos)
          val trees = parseTrees(xs)
          (PackageDef(pid, trees, pos), Nil)
      }
    }
    



    private def parsePackage(tokenList: TokenList): PackageDef = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Package), tokenList)
      val (pid, rest2) = parseSelectOrIdent(rest1)
      val rest3 = parseOrReport(tokens.Punctuation(tokens.Semi), rest2)
      val trees = parseTrees(rest3)
      PackageDef(pid, trees, posOfHead(tokenList))
    }
    
    private def parseTrees(tokenList: TokenList): List[Tree] = {
      tokenList match {
        // case tokens.ScalaBlock(verbatim, pos) :: xs => 
          // (ScalaBlock(verbatim, pos), xs)
        case tokens.CommentBlock(verbatim, pos) :: xs =>
          Comment(BlockComment, verbatim, pos) :: parseTrees(xs)
        case tokens.CommentLine(verbatim, pos) :: xs =>
          Comment(LineComment, verbatim, pos) :: parseTrees(xs)
        // case tokens.Keyword(tokens.Package) :: xs =>
          // parsePackage(tokenList)
        case tokens.Keyword(tokens.Import) :: xs =>
          val (tree, rest) = parseImport(tokenList)
          tree :: parseTrees(rest)
        case tokens.Keyword(tokens.Plugin) :: xs =>
          val (tree, rest) = parsePlugin(tokenList)
          tree :: parseTrees(rest)
        // FIXME: Broken or not implemented
        case tokens.Keyword(tokens.If) :: xs =>
          val (tree, rest) = parseIf(tokenList)
          tree :: parseTrees(rest)
        case (tokens.Keyword(tokens.Def) | 
              tokens.Keyword(tokens.Private)) :: xs =>
          val (trees, rest) = parseDef(tokenList)
          trees ++ parseTrees(rest)
        case Nil => Nil
        case x :: xs => 
          // TODO: another naiive error recovery, test it then decide if it is good
          // or not
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
      val rest3 = parseOrReport(tokens.Punctuation(tokens.Semi), rest2)
      (Import(qual, posOfHead(tokenList)), rest3)
    }

    private def parsePlugin(tokenList: TokenList): (PluginDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Plugin), tokenList)
      val (name, rest2) = parseId(rest1)
      val (args, rest3) = parseIDArgs(rest2)
      val (body, rest4) = parsePluginBody(rest3)
      (PluginDef(name, args, body, posOfHead(tokenList)), rest4)
    }

    private def pasePhase(tokenList: TokenList): (PhaseDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Phase), tokenList)
      val (id, rest2) = parseId(rest1)
      val rest3 = parseOrReport(tokens.Punctuation(tokens.LParan), rest2)
      val (lit, rest4) = parseLiteral(rest3)
      val rest5 = parseOrReport(tokens.Punctuation(tokens.RParan), rest4)
      val (body, pre, perf, rest6) = parsePhaseBody(rest5) 
      (PhaseDef(id, lit.valueAsString, pre, perf, body, posOfHead(tokenList)), rest6)
    }


    private def parseDef(tokenList: TokenList): (List[DefDef], TokenList) = {
      val (mod, rest1) = parseModifier(tokenList)
      val rest2 = parseOrReport(tokens.Keyword(tokens.Def), rest1)
      rest2 match {
        case tokens.Punctuation(tokens.LParan) :: xs =>
          parseMultiDef(tokenList)
        case _ =>
          val (name, rest3) = parseId(rest2)
          val (tparams, rest4) = parseGenerics(rest3)
          val (params, rest5) = parseParams(rest4)
          val rest6 = parseOrReport(tokens.Punctuation(tokens.Colon), rest5)
          val (tpe, rest7) = parseType(rest6)
          val rest8 = parseOrReport(tokens.Punctuation(tokens.Assign), rest7)
          val (rhs, rest9) = parseExpression(rest8)
          val defdef = DefDef(mod, name, tparams, params, tpe, 
            rhs, posOfHead(tokenList))
          (List(defdef), rest9)
      }
    }


    private def parseMultiDef(tokenList: TokenList): (List[DefDef], TokenList) = {
      
      @tailrec def toDefs(mod: Modifier, ids: List[DefDef], rhs: Expression, acc: List[DefDef]): 
          List[DefDef] = {
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

      val (mod, rest1) = parseModifier(tokenList)
      val rest2 = parseOrReport(tokens.Keyword(tokens.Def), rest1)
      val rest3 = parseOrReport(tokens.Punctuation(tokens.LParan), rest2)
      val (defs, rest4) = parseParams(rest3)
      // defs match {
      //   case Nil =>
      //     reporter.report("(", ":", opsOfHead(rest3), BAD_TOKEN)
      //   case x :: Nil =>
      //     reporter.report("Pattern matching needs to be at least of length 2", opsOfHead(rest3))
      //   case _ => ()
      // }
      val rest5 = parseOrReport(tokens.Punctuation(tokens.Assign), rest4)
      val (rhs, rest6) = parseExpression(rest5)
      (toDefs(mod, defs, rhs, Nil), rest6)
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
        val (tpe, rest3) = parseType(rest2)
        val m = DefDef(Modifier(Modifier.PARAM), id, Nil, Nil, tpe, EmptyExpression, id.pos)
        (m, rest3)
      }
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      sequenceHelper[DefDef](rest1, Nil, parseParam, tokens.Punctuation(tokens.RParan))
    }

    private def parseSimpleType(tokenss: TokenList): (SimpleType, TokenList) = {
      def helperTArgs(tlist: TokenList): (List[TypeTree], TokenList) = {
        sequenceHelper[TypeTree](tokenss, Nil, parseType, tokens.Punctuation(tokens.RBracket))
      }
      val (id, rest1) = parseSelectOrIdent(tokenss)
      val (targs, rest2) = rest1 match {
        case tokens.Punctuation(tokens.LBracket) :: xs =>
          helperTArgs(xs)
        case _ =>
        (Nil, rest1)
      }
      (SimpleType(id, targs, id.pos), rest2)
    }


    private def parseType(tokenList: TokenList): (TypeTree, TokenList) = {
      def parseProductType(tokenss: TokenList): (List[TypeTree], TokenList) = {
        sequenceHelper[TypeTree](tokenss, Nil, parseType, tokens.Punctuation(tokens.RParan))
      }
      
      
      tokenList match {
        case tokens.Punctuation(tokens.LParan) :: xs =>
          val (product, rest1) = parseProductType(xs)
          rest1 match {
            case tokens.Punctuation(tokens.Arrow) :: xs =>
              val (r, rest2) = parseType(xs)
              (FunctionType(product, r, posOfHead(tokenList)), rest2)
            case _ =>
              (ProductType(product, posOfHead(tokenList)), rest1)
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

    private def parseIDArgs(tokenList: TokenList): (List[SelectOrIdent], TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      sequenceHelper[SelectOrIdent](rest1, Nil, parseSelectOrIdent, tokens.Punctuation(tokens.RParan))
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
            (SimpleType(Ident("Nothing", pos), Nil, pos), rest1)
        }
        val (ubound, rest3) = rest2 match {
          case tokens.Punctuation(tokens.SubType) :: xs =>
            parseType(xs)
          case _ =>
            val pos = posOfHead(rest2)
            (SimpleType(Ident("Any", pos), Nil, pos), rest2)
        }

        val tparam = TParamDef(id, lbound, ubound, id.pos)
        (tparam, rest3)
      }

      tokenList match {
        case tokens.Punctuation(tokens.LBracket) :: xs =>
          sequenceHelper[TParamDef](xs, Nil, parseTParam, tokens.Punctuation(tokens.RBracket))
        case _ => (Nil, tokenList)
      }
    }

    private def parseModifier(tokenList: TokenList): (Modifier, TokenList) = {
      tokenList match {
        case tokens.Keyword(tokens.Private) :: xs =>
          (Modifier(Modifier.PRIVATE), xs)
        case xs => (Modifier(Modifier.PUBLIC), xs)
      }
    }


    private def parseCheckOrTransform(tokenList: TokenList): (DefDef, TokenList) = {
      val pos = posOfHead(tokenList)
      val (name, rest1) = tokenList match {
        case tokens.Keyword(tokens.Transform) :: xs => (Ident(Names.TRANSFORMER, pos), xs)
        case tokens.Keyword(tokens.Check) :: xs => (Ident(Names.CHECKER, pos), xs)
        case xs =>
          reporter.report(Names.TRANSFORMER, pos, BAD_TOKEN)
          (Ident(Names.TRANSFORMER, pos), xs)
      }
      val (rhs, rest2) = parseExpression(rest1)
      val tpe = SimpleType(Ident(Names.TREE_TYPE, pos), Nil, pos)
      val mod = {
        if(name.name == Names.TRANSFORMER) 
          Modifier(Modifier.TRANSFORMER) 
        else
          Modifier(Modifier.CHECKER) 
      }
      val m = DefDef(mod, name, Nil, Nil, tpe, rhs, pos)
      (m, rest2)
    }

    private def parsePreamble(tokenList: TokenList): (Assign, TokenList) = {
      val pos = posOfHead(tokenList)
      val (lhs, rest1) = tokenList match {
        case tokens.Keyword(tokens.RunsBefore) :: xs => (Ident(Names.RUNS_BEFORE, pos), xs)
        case tokens.Keyword(tokens.RunsAfter) :: xs => (Ident(Names.RUNS_AFTER, pos), xs)
        case tokens.Keyword(tokens.RunsRightAfter) :: xs => (Ident(Names.RUNS_RIGHT_AFTER, pos), xs)
        case xs =>
          reporter.report(Names.RUNS_AFTER, pos, BAD_TOKEN)
          (Ident(Names.RUNS_AFTER, pos), xs)
      }
      val (rhs, rest2) = parseArgs(rest1)
      val rhsPos = rhs match {
        case Nil => pos
        case x :: xs => x.pos
      }
      val rest3 = parseOrReport(tokens.Punctuation(tokens.Semi), rest2)
      val assign = Assign(lhs, Apply(Ident("List", rhsPos), Nil, rhs, rhsPos), pos)
      (assign, rest3)
    }
    private def parsePluginBody(tokenList: TokenList): (List[DefDef], TokenList) = {
      def helper(tokenss: TokenList): (List[DefDef], TokenList) = {
        tokenss match {
          case (tokens.Keyword(tokens.Def) | 
              tokens.Keyword(tokens.Private)) :: xs =>
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

    private def parsePhaseBody(tokenList: TokenList): (List[Tree], List[Assign], 
          DefDef, TokenList) = {
      // TODO: Make this tailrec
      def helper(tokenss: TokenList, body: List[Tree], preambles: List[Assign],
        performer: Option[DefDef]): (List[Tree], List[Assign], Option[DefDef], TokenList) = {

        tokenss match {
          case tokens.Punctuation(tokens.RCurly) :: xs =>
            (body, preambles, performer, xs)
          case (tokens.Keyword(tokens.RunsBefore) |
                tokens.Keyword(tokens.RunsAfter) |
                tokens.Keyword(tokens.RunsRightAfter)) :: xs => 
            val (pre, r) = parsePreamble(tokenss)
            helper(r, body, pre :: preambles, performer)
          case (tokens.Keyword(tokens.Def) | 
              tokens.Keyword(tokens.Private)) :: xs =>
            val (ms, r) = parseDef(tokenss)
            helper(r, ms ++ body, preambles, performer)
          case (tokens.Keyword(tokens.Transform) |
               tokens.Keyword(tokens.Check)) :: xs if performer == None => 
            val (p, r) = parseCheckOrTransform(tokenss)
            helper(r, body, preambles, Some(p))
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
          val dummyType = SimpleType(dummyName, Nil, dummyName.pos)
          val dummy = DefDef(Modifier(Modifier.PRIVATE), 
              dummyName, Nil, Nil, dummyType, EmptyExpression, pos)
          reporter.report("}", Names.TRANSFORMER, pos, BAD_TOKEN)
          (body, preambles, dummy, rest)
      }
    }


    @tailrec private def parseSelectOrIdent(tokenList: TokenList, acc: List[Ident]):
        (List[Ident], TokenList) = {

      val (id, rest) =  parseId(tokenList)
      rest match {
        case tokens.Punctuation(tokens.Dot) :: 
          tokens.Punctuation(tokens.Underscore) :: xs =>
          ((id :: acc).reverse, rest)
        case tokens.Punctuation(tokens.Dot) :: xs =>
          parseSelectOrIdent(xs, id :: acc)
        case _ => (acc, rest)
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
          val dummyName = Ident(Names.uniqueName(Names.DUMMY_NAME), posOfHead(tokenList))
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

    private def parseCases(tokenList: TokenList): (List[CaseDef], TokenList) = {
      def parseCase(tokenss: TokenList): (CaseDef, TokenList) = {
        // TODO: Implement this
        null
      }
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

    private def parseMatch(expr: Expression, tokenList: TokenList): (Match, TokenList) = {
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
          parseExpression(xs)
        case _ =>
          (EmptyExpression, rest4)
      }
      (Try(expr, cases, fnly, expr.pos), rest5)
    }

    private def parseBlock(tokenList: TokenList): (Block, TokenList) = {
      @tailrec def helper(tokenss: TokenList, acc: List[PositionedTree]): 
          (Block, TokenList) = {
        val (stmts, rest1) = tokenss match {
          case (tokens.Keyword(tokens.Private) |
            tokens.Keyword(tokens.Def)) :: xs => 
            parseDef(tokenss)
          case _ =>
            val (expr, rest1) = parseExpression(tokenss)
            // FIXME: Not all trees need to end with a semi colon
            val rest2 = parseOrReport(tokens.Punctuation(tokens.Semi), rest1)
            (List(expr), rest2)
        }
        rest1 match {
          case tokens.Punctuation(tokens.RCurly) :: xs =>
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
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LCurly), tokenList)
      helper(rest1, Nil)
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

    

    private def parseUnary(tokenList: TokenList, pos: Position, uop: UniOp): (Unary, TokenList) = {
      val (expr, rest1) = parseExpression(tokenList)
      (Unary(uop, expr, pos), rest1)
    }


    private def parseBinary(tokenList: TokenList, lhs: Expression, pos: Position, bop: BinOp): 
        (Binary, TokenList) = {
      val (rhs, rest1) = parseExpression(tokenList)
      (Binary(lhs, bop, rhs, pos), rest1)
    }
    

    private def parseFun(tokenList: TokenList): (Function, TokenList) = {
      val (params, rest1) = parseParams(tokenList)
      val rest2 = parseOrReport(tokens.Punctuation(tokens.Arrow), rest1)
      val (expr, rest3) = parseExpression(rest2)
      (Function(params, expr, posOfHead(tokenList)), rest3)
    }

    private def parseSelectExprOrIdent(tokenList: TokenList, 
        expr: Expression): (SelectOrIdent, TokenList) = {
      // TODO: make sure this call terminates
      val (ids, rest1) = parseSelectOrIdent(tokenList, Nil)
      val select = toSelect(expr, ids)
      (select, rest1)
    }

    private def parseRecord(tokenList: TokenList): (Expression, TokenList) = {
      val rest1 = parseOrReport(tokens.Punctuation(tokens.LParan), tokenList)
      val (exprs, rest2) = sequenceHelper[Expression](rest1, Nil, parseExpression, 
                tokens.Punctuation(tokens.RParan))
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
     * </ul>
     */
    // TODO: Implement
    // TODO: Turn this into a tail recursive function
    private def parseExpression(tokenList: TokenList): (Expression, TokenList) = {
      tokenList match {
        case tokens.Punctuation(tokens.Minus) :: xs =>
          parseUnary(xs, posOfHead(tokenList), Negative)
        case tokens.Punctuation(tokens.Not) :: xs =>
          parseUnary(xs, posOfHead(tokenList), Not)
        case tokens.Keyword(tokens.If) :: xs =>
          parseIf(tokenList)
        case tokens.Keyword(tokens.Try) :: xs =>
          parseTry(tokenList)
        case tokens.Punctuation(tokens.LParan) :: xs =>
          parseRecord(tokenList)
        case tokens.Punctuation(tokens.LCurly) :: xs =>
          parseBlock(tokenList)
        case tokens.Literal(_, _) :: xs =>
          parseLiteral(tokenList)
        case _ => null
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
      val removedExtraNL = tokenList.map(removeExtraNL(_))
      removedExtraNL.map(normalize(_))
    }

    private def removeExtraNL(tokenList: TokenList): TokenList = {
      tokenList match {
        case Nil => Nil
        case tokens.Punctuation(tokens.NL) :: tokens.Punctuation(tokens.NL) :: xs => 
          removeExtraNL(tokens.Punctuation(tokens.NL) :: xs)
        case x :: xs => x :: removeExtraNL(xs)
      }
    }

    private def normalize(tokenList: TokenList): TokenList = {
      tokenList match {
        case Nil => Nil
        case tokens.EmptyToken :: xs => normalize(xs)
        case x :: tokens.Punctuation(tokens.NL) :: y :: xs 
            if canEnd(x) && canStart(y) =>
          x :: tokens.Punctuation(tokens.Semi) :: normalize(xs)
        case x :: tokens.Punctuation(tokens.NL) :: y :: xs =>
          x :: y :: normalize(xs)
        case tokens.Punctuation(tokens.NL) :: xs => normalize(xs)
        case x :: xs =>
          x :: normalize(xs)
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
                tokens.Def | tokens.Private | tokens.Super |
                tokens.This | tokens.Throw => true
            case _ => false
          }
        case tokens.Punctuation(punc) =>
          punc match {
            case tokens.LCurly | tokens.LParan | tokens.Minus |
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
          val pos = Position(file, col -read.length, row)
          val posSym = pos.copy(col = col)
          identify(read, pos) :: identify(x, y, posSym) :: lexify(xs)(file, col + 2)
        case '\'' :: x :: '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posChar = pos.copy(col = col)
          identify(read, pos) :: tokens.Literal(x, posChar) :: 
              lexify(xs)(file, col + 3)
        case '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          reporter.report("'", pos, BAD_TOKEN)
          identify(read, pos) :: lexify(xs)(file, col + 1)
        case '\"' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posStr = pos.copy(col = col)
          val (rest, strLit, nrow, ncol) = 
            readStringLiteral(xs, "", col + 1, row)(pos)
          identify(read, pos) :: strLit :: lexify(rest, ncol, "")(file, nrow)
        case x :: xs if isIntegral(read + x) =>
          lexify(xs, col + 1, read + x)(file, row)
        case '.' :: xs if isIntegral(read) =>
          lexify(xs, col + 1, read + '.')(file, row)
        case 'e' :: '-' :: xs if isDecimal(read) =>
          lexify(xs, col + 2, read + "e-")(file, row)
        case 'e' :: '+' :: xs if isDecimal(read) =>
          lexify(xs, col + 2, read + "e+")(file, row)
        case 'e' :: xs if isDecimal(read) =>
          lexify(xs, col + 1, read + "e")(file, row)
        case x :: xs if isDecimal(read + x) =>
          lexify(xs, col + 1, read + x)(file, row)
        case  xs if isIntegral(read) =>
          val pos = Position(file, col - read.length, row)
          tokens.Literal(read.toInt, pos) :: lexify(xs, col, "")(file, row)
        case  xs if isDecimal(read) =>
          val pos = Position(file, col - read.length, row)
          tokens.Literal(read.toDouble, pos) :: lexify(xs, col, "")(file, row)
        case ('\n' | '\r') :: xs => 
          val pos = Position(file, col - read.length, row)
          val posNL = pos.copy(col = col)
          identify(read, pos) :: tokens.Punctuation(tokens.NL, posNL) :: 
              lexify(xs)(file, row + 1)
        case '/' :: '*' :: xs =>
          val pos = Position(file, col, row)
          val prevPos = pos.copy(col = col - read.length)
          val (rest, block, ncol, nrow) = 
              readCommentBlock(xs, "", col + 2, row)(pos)
          identify(read, prevPos) :: block :: lexify(rest, ncol, "")(file, nrow)
        // case '.' :: '.' :: '/' :: xs =>
        //   val pos = Position(file, col, row)
        //   val prevPos = pos.copy(col = col - read.length)
        //   val (rest, block, ncol, nrow) = 
        //       readScalaBlock(xs, "", col + 3, row)(pos)
        // identify(read, prevPos) :: block :: lexify(rest, ncol, "")(file, nrow)
        case '/' :: '/' :: xs =>
          val pos = Position(file, col, row)
          val prevPos = pos.copy(col = col - read.length)
          identify(read, pos) :: List(tokens.CommentLine(xs.mkString, pos))
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


    @tailrec private def readStringLiteral(chars: List[Char], read: String, 
      col: Int, row: Int)(implicit pos: Position): 
          (List[Char], tokens.Literal, Int, Int) = {
      chars match {
        case Nil =>
          reporter.report("\"", LINE_FEED, pos.copy(col = col), BAD_TOKEN)
          (Nil, tokens.Literal(read, pos), col, row)
        case '\\' :: '"' :: xs =>
          readStringLiteral(xs, read + "\\\"", col + 2, row)
        case '\n' :: xs =>
          reporter.report("\"", LINE_FEED, pos.copy(col = col + 1), BAD_TOKEN)
          (xs, tokens.Literal(read, pos), 1, row + 1)
        case '"' :: xs =>
          (xs, tokens.Literal(read, pos), col + 1, row)
        case x :: xs =>
          readStringLiteral(xs, read + x, col + 1, row)
      }
    }

    @tailrec private def readCommentBlock(chars: List[Char], read: String, 
      col: Int, row: Int)(implicit pos: Position): 
        (List[Char], tokens.CommentBlock, Int, Int) = {
      chars match {
        case Nil => (Nil, tokens.CommentBlock(read, pos), col, row)
        case '*' :: '/' :: xs =>
          (xs, tokens.CommentBlock(read, pos), col + 2, row)
        case ('\n' | '\r') :: xs =>
          readCommentBlock(xs, read, 1, row + 1)
        case x :: xs =>
          readCommentBlock(xs, read + x, col + 1, row)
      }
    }

    private def isSeparator(x: Char): Boolean = {
      // A list of the punctuations that can separate a word
      val separators = " \n\r{}()[]=+-/*%<>!|&'\"_.;:\\,@"
      separators.contains(x) 
    }


    private def isIntegral(str: String): Boolean = {
      str match {
        case "" => false
        case s => s.foldLeft(true)((z, y) => z && y.isDigit)
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
        case "private" => tokens.Keyword(tokens.Private, pos)
        case "this" => tokens.Id("this", pos)
        case "super" => tokens.Id("super", pos)
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
    case object LCurly extends Punctuations
    case object RCurly extends Punctuations
    case object LParan extends Punctuations
    case object RParan extends Punctuations
    case object LBracket extends Punctuations
    case object RBracket extends Punctuations


    // mathmatical symbols
    case object Assign extends Punctuations
    case object Plus extends Punctuations
    case object Minus extends Punctuations
    case object Div extends Punctuations
    case object Mul extends Punctuations
    case object Mod extends Punctuations

    // logical symbols
    case object LT extends Punctuations
    case object GT extends Punctuations
    case object Not extends Punctuations


    // composed symbols
    case object Le extends Punctuations
    case object Ge extends Punctuations
    case object Eq extends Punctuations
    case object Ne extends Punctuations
    case object SubType extends Punctuations
    case object SuperType extends Punctuations
    case object Arrow extends Punctuations
    case object And extends Punctuations
    case object Or extends Punctuations


    // other symbols
    case object Underscore extends Punctuations
    case object Dot extends Punctuations
    case object Semi extends Punctuations
    case object Colon extends Punctuations
    // case object BackSlash extends Punctuations
    case object Coma extends Punctuations
    case object At extends Punctuations
    case object NL extends Punctuations
    case object Pipe extends Punctuations

    
    // Lombrello keywords
    sealed abstract class Keywords
    case object RunsAfter extends Keywords
    case object RunsRightAfter extends Keywords
    case object RunsBefore extends Keywords
    case object Import extends Keywords
    case object If extends Keywords
    case object Else extends Keywords
    case object Match extends Keywords
    case object Package extends Keywords
    case object Plugin extends Keywords
    case object Phase extends Keywords
    case object Transform extends Keywords
    case object Check extends Keywords
    case object Def extends Keywords
    case object Case extends Keywords
    case object Tree extends Keywords   
    case object Private extends Keywords
    case object Super extends Keywords
    case object This extends Keywords
    case object Throw extends Keywords
    case object Try extends Keywords
    case object Catch extends Keywords
    case object Finally extends Keywords
    case object New extends Keywords

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

      override def hashCode: Int = keyword.hashCode
      override def equals(that: Any): Boolean = {
        that match {
          case null => false
          case `keyword` => true
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
      override def hashCode: Int = kind.hashCode
      override def equals(that: Any): Boolean = {
        that match {
          case null => false
          case `kind` => true
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
