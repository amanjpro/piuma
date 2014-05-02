package ch.usi.inf.l3.lombrello.dsl.parser



/**
 * @author Amanj Sherwany
 * @date 29 Apr 2014
 */

import java.io.File
import scala.io.Source
import ch.usi.inf.l3.lombrello
import lombrello.dsl._
import scala.annotation.tailrec

trait Parsers { self: Compiler =>

  val tokens = new Tokens {}

  type TokenList = List[tokens.Token]

  // TODO: Implement
  def report(expected: tokens.Token, found: tokens.Token): Nothing = {
    throw new Exception("Here")
  }
  // TODO: Implement
  def report(found: tokens.Token): Nothing = {
    throw new Exception("Here")
  }

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
          parsePackage(tokenList)
        // FIXME: Broken or not implemented
        case tokens.Keyword(tokens.If) :: xs =>
          parseIf(tokenList)
        case (tokens.Keyword(tokens.Def) | 
              tokens.Keyword(tokens.Private)) :: xs =>
          parseDef(tokenList)
        case Nil => (NoTree, Nil)
        case _ => (NoTree, Nil)
      }
    }


    private def parsePackage(tokenList: TokenList): (PackageDef, TokenList) = {
      val rest1 = parseOrReport(tokens.Keyword(tokens.Package), tokenList)
      val (pid, rest2) = parseSelectOrIdent(rest1)
      // TODO: Implement this
      (PackageDef(pid, Nil, posOfHead(tokenList)), rest2)
    }
    
    private def parseDef(tokenList: TokenList): (DefDef, TokenList) = {
      val (isPrivate, rest1) = parseModifier(tokenList)
      val rest2 = parseOrReport(tokens.Keyword(tokens.Def), rest1)
      val (name, rest3) = parseId(rest2)
      val (tparams, rest4) = parseGenerics(rest3)
      val (params, rest5) = parseParams(rest4)
      val rest6 = parseOrReport(tokens.Punctuation(tokens.Colon), rest5)
      val (tpe, rest7) = parseId(rest6)
      val rest8 = parseOrReport(tokens.Punctuation(tokens.Assign), rest7)
      val (rhs, rest9) = parseExpression(rest8)
      val defdef = DefDef(isPrivate, name, tparams, params, tpe, 
        rhs, posOfHead(tokenList))
      (defdef, rest9)
    }

    // TODO: Implement
    private def parseId(tokenList: TokenList): (Ident, TokenList) = {
      null
    }

    // TODO: Implement
    private def parseParams(tokenList: TokenList): (List[DefDef], TokenList) = {
      null
    }

    // TODO: Implement
    private def parseGenerics(tokenList: TokenList): 
        (List[TParamDef], TokenList) = {
      null
    }

    private def parseModifier(tokenList: TokenList): (Boolean, TokenList) = {
      tokenList match {
        case tokens.Keyword(tokens.Private) :: xs =>
          (true, xs)
        case xs => (false, xs)
      }
    }

    private def parseSelectOrIdent(tokenList: TokenList): 
      (SelectOrIdent, TokenList) = {
      // TODO: Implement
      null
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

    // TODO: Implement
    private def parseBlock(tokenList: TokenList): (Block, TokenList) = {
      null
    }


    
    private def parseOrReport(expected: tokens.Token, 
      tokenList: TokenList): TokenList = {
      tokenList match {
        case `expected` :: rest => rest
        case x :: rest =>
          report(expected, x)
        case Nil =>
          report(expected, tokens.EmptyToken) 
          Nil
      }
    }

    

    

    /**
     * An expression is one of the following:
     * <ul>
     *  <li> Ident
     *  <li> Select
     *  <li> Literal
     *  <li> Apply
     *  <li> Binary Operation
     *  <li> Unary Operation
     *  <li> If
     *  <li> Match
     *  <li> Block
     *  <li> Fun
     * </ul>
     */
    // TODO: Implement
    private def parseExpression(tokenList: TokenList): (Expression, TokenList) = {
      null
    }


    private def posOfHead(tokens: TokenList): Position = {
      val dummy = Position(new File(""), 0, 0)
      tokens match {
        case Nil => dummy
        case x :: xs => x.position.getOrElse(dummy)
      }
    }
  }


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
                tokens.Quote | tokens.DoubleQuote | tokens.Underscore =>
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
                tokens.Else | tokens.Case | tokens.Match | tokens.Package |
                tokens.Plugin | tokens.New | tokens.Phase | tokens.Transform |
                tokens.Check | tokens.Def | tokens.Private | tokens.Super |
                tokens.This | tokens.Throw => true
            case _ => false
          }
        case tokens.Punctuation(punc) =>
          punc match {
            case tokens.LCurly | tokens.LParan | tokens.LBracket |
                tokens.Quote | tokens.DoubleQuote | tokens.Minus |
                tokens.Not | tokens.At =>
              true
            case _ => false
          }
        case _ => true
      }
    }

  }

  class Lexer extends Phase {
    type InputType = List[File]
    type OutputType = List[TokenList]
    val name: String = "lexer"

    val runsAfter: Option[String] = None

    def run(files: InputType): OutputType = {
      files.map(lexify(_))
    } 



    private def lexify(file: File): TokenList = {
      val chars = readFile(file)
      lexify(chars)(file, 1)
    }

    // TODO: Make this tail recursive
    private def lexify(chars: List[Char], col: Int = 1, 
        read: String = "")(implicit file: File, row: Int): TokenList = {
      chars match {
        case Nil => Nil
        case '\'' :: x :: '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          val posChar = pos.copy(col = col)
          identify(read, pos) :: tokens.Literal[Char](x, posChar) :: 
              lexify(xs)(file, col + 3)
        case '\'' :: xs =>
          val pos = Position(file, col - read.length, row)
          report(tokens.Punctuation(tokens.Quote))
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
          (List[Char], tokens.Literal[String], Int, Int) = {
      chars match {
        case Nil =>
          report(tokens.Punctuation(tokens.DoubleQuote), 
            tokens.Punctuation(tokens.NL))
          (Nil, tokens.Literal[String](read, pos), col, row)
        case '\\' :: '"' :: xs =>
          readStringLiteral(xs, read + "\\\"", col + 2, row)
        case '\n' :: xs =>
          report(tokens.Punctuation(tokens.NL))
          (xs, tokens.Literal[String](read, pos), col + 1, row)
        case '"' :: xs =>
          (xs, tokens.Literal[String](read, pos), col + 1, row)
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
        case "super" => tokens.Keyword(tokens.Super, pos)
        case "this" => tokens.Keyword(tokens.This, pos)
        case "throw" => tokens.Keyword(tokens.Throw, pos)
        case "new" => tokens.Keyword(tokens.New, pos)
        case "true" => tokens.Literal(true, pos)
        case "false" => tokens.Literal(false, pos)
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
        case '&' => tokens.Punctuation(tokens.Amp, pos)
        case '\'' => tokens.Punctuation(tokens.Quote, pos)
        case '"' => tokens.Punctuation(tokens.DoubleQuote, pos)
        case '_' => tokens.Punctuation(tokens.Underscore, pos)
        case '.' => tokens.Punctuation(tokens.Dot, pos)
        case ';' => tokens.Punctuation(tokens.Semi, pos)
        case ':' => tokens.Punctuation(tokens.Colon, pos)
        // case '\\' => tokens.Punctuation(tokens.BackSlash, pos)
        case ',' => tokens.Punctuation(tokens.Coma, pos)
        case '@' => tokens.Punctuation(tokens.At, pos)
      }
    }

    private def readFile(file: File): List[Char] = {
      Source.fromFile(file).getLines.mkString.toList
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
    case object Pipe extends Punctuations
    case object Amp extends Punctuations

    // quotations
    case object Quote extends Punctuations
    case object DoubleQuote extends Punctuations

    // other symbols
    case object Underscore extends Punctuations
    case object Dot extends Punctuations
    case object Semi extends Punctuations
    case object Colon extends Punctuations
    // case object BackSlash extends Punctuations
    case object Coma extends Punctuations
    case object At extends Punctuations
    case object NL extends Punctuations

    
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
    case class Literal[T](value: T, pos: Position) extends PositionedToken

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
        new Keyword(kind, Position(new File(""), 0, 0))
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
        new Punctuation(kind, Position(new File(""), 0, 0))
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
