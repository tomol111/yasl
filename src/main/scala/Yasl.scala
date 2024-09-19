package yasl

import util.boundary, boundary.break
import util.matching.Regex
import util.{Try, Success, Failure}


// Interpreter
// ===========


def exec(ast: AST, env: Environment): Unit = ast match
  case LetStmt(Name(target), expr) =>
    env(target) = eval(expr, env)
  case expr: Expr =>
    eval(expr, env)


def eval(expr: Expr, env: Environment): YaslValue = expr match
  case Const(value) => value

  case Name(variable) => env(variable)

  case Unary(op, operand) => op match
    case Plus => eval(operand, env) match
      case value: Double => value
      case _ => ???
    case Minus => eval(operand, env) match
      case value: Double => -value
      case _ => ???
    case Not => eval(operand, env) match
      case value: Boolean => !value
      case _ => ???

  case Binary(left, op, right) => (eval(left, env), eval(right, env)) match
    case (leftVal: Double, rightVal: Double) => op match
      case Plus => leftVal + rightVal
      case Minus => leftVal - rightVal
      case Star => leftVal * rightVal
      case Slash => leftVal / rightVal
      case Caret => math.pow(leftVal, rightVal)
      case EqEqual => leftVal == rightVal
      case NotEqual => leftVal != rightVal
      case Less => leftVal < rightVal
      case Greater => leftVal > rightVal
      case LessEqual => leftVal <= rightVal
      case GreaterEqual => leftVal >= rightVal
    case (_, _) => ???

  case LazyBinary(left, op, right) => op match
    case And => eval(left, env).asInstanceOf[Boolean] && eval(right, env).asInstanceOf[Boolean]
    case Or => eval(left, env).asInstanceOf[Boolean] || eval(right, env).asInstanceOf[Boolean]

  case Block(stmts, expr) =>
    stmts.foreach(exec(_, env))
    expr.map(eval(_, env)).getOrElse(YaslNil)

  case IfExpr(condition, thenBranch, elseBranch) =>
    eval(condition, env) match
      case true =>
        eval(thenBranch, env)
      case false =>
        elseBranch.map(eval(_, env)).getOrElse(YaslNil)
      case _ => ???


type Environment = collection.mutable.Map[String, YaslValue]
object Environment:
  def apply(items: (String, YaslValue)*): Environment =
    collection.mutable.Map[String, YaslValue](items*)


type YaslValue = Double | Boolean | YaslNil.type

object YaslNil


// Parser
// ======

/*
 * block        ::= "{" block_content "}"
 * block_content::= {statement} [expression]
 *
 * statement    ::= "let" IDENTIFIER "=" expression ";"
 *                | expression ";"
 *
 * expression   ::= if_expr
 *                | or_expr
 *
 * -- `if`'s condition could also be `expression`
 * if_expr      ::= "if" or_expr block ["else" (if_expr | block)]
 *
 * or_expr      ::= [or_expr "or"] and_expr
 * and_expr     ::= [and_expr "or"] negation
 * negation     ::= ["not"] comparison
 * comparison   ::= term [(">"|">="|"<"|"<="|"!="|"==") term]
 * term         ::= ["+"|"-"] factor | term ("+"|"-") factor
 * factor       ::= [factor ("*"|"/")] power
 * power        ::= primary ["^" power]
 * primary      ::= IDENTIFIER | NUMBER | "true" | "false" | "(" expression ")"
 */


def parse(tokens: List[Token]): Block =
  val (ast, rest) = parseBlockContent(tokens)
  assert(rest == Nil)
  ast


def parseBlock(tokens: List[Token]): (Block, List[Token]) =
  val (block, rest) = parseBlockContent(tokens.consume(LBrace))
  (block, rest.consume(RBrace))


def parseBlockContent(tokens: List[Token]): (Block, List[Token]) =
  val stmts = collection.mutable.ListBuffer.empty[Stmt]
  def go(tokens: List[Token]): (Block, List[Token]) =
    Try(parseStatement(tokens)) match
      case Success(stmt, rest) =>
        stmts += stmt
        go(rest)
      case Failure(_) =>
        Try(parseExpression(tokens)) match
          case Success(expr, rest) =>
            (Block(stmts.toList, Some(expr)), rest)
          case Failure(_) =>
            (Block(stmts.toList), tokens)
  go(tokens)


def parseStatement(tokens: List[Token]): (Stmt, List[Token]) = tokens match
  case Token(Let, _, _) :: Token(Identifier, target, _) :: Token(Equal, _, _) :: tailWithValue =>
    val (expr, rest) = parseExpression(tailWithValue)
    (LetStmt(Name(target), expr), rest.consume(Colon))
  case tokens =>
    val (expr, rest) = parseExpression(tokens)
    (expr, rest.consume(Colon))


def parseExpression(tokens: List[Token]): (Expr, List[Token]) =
  if tokens.head.typ == If
    then parseIfExpr(tokens)
    else parseOrExpr(tokens)


def parseIfExpr(tokens: List[Token]): (Expr, List[Token]) =
  val (condition, tailWithThenBranch) = parseOrExpr(tokens.consume(If))
  val (thenBranch, tail) = parseBlock(tailWithThenBranch)
  tail match
    case Token(Else, _, _) :: tailWithElseBranch =>
      val (elseBranch, rest) =
        if tailWithElseBranch.head.typ == If
          then parseIfExpr(tailWithElseBranch)
          else parseBlock(tailWithElseBranch)
      (IfExpr(condition, thenBranch, Some(elseBranch)), rest)
    case rest =>
      (IfExpr(condition, thenBranch), rest)


def parseOrExpr(tokens: List[Token]): (Expr, List[Token]) =
  val (expr, tail) = parseAndExpr(tokens)
  _parseOrExpr(expr, tail)

def _parseOrExpr(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(Or, _, _) :: tailWithOperand =>
      val (right, tail) = parseAndExpr(tailWithOperand)
      _parseOrExpr(LazyBinary(left, Or, right), tail)
    case rest =>
      (left, rest)


def parseAndExpr(tokens: List[Token]): (Expr, List[Token]) =
  val (expr, tail) = parseNegation(tokens)
  _parseAndExpr(expr, tail)

def _parseAndExpr(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(And, _, _) :: tailWithOperand =>
      val (right, tail) = parseNegation(tailWithOperand)
      _parseAndExpr(LazyBinary(left, And, right), tail)
    case rest =>
      (left, rest)


def parseNegation(tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(Not, _, _) :: tail =>
      val (expr, rest) = parseComparison(tail)
      (Unary(Not, expr), rest)
    case _ =>
      parseComparison(tokens)


type ComparisonOp = EqEqual.type | NotEqual.type | Less.type | Greater.type | LessEqual.type | GreaterEqual.type

def parseComparison(tokens: List[Token]): (Expr, List[Token]) =
  val (left, tail) = parseTerm(tokens)
  tail match
    case Token(op: ComparisonOp, _, _) :: tailWithRightTerm =>
      val (right, rest) = parseTerm(tailWithRightTerm)
      (Binary(left, op, right), rest)
    case _ =>
      (left, tail)


type TermOp = Plus.type | Minus.type

def parseTerm(tokens: List[Token]): (Expr, List[Token]) =
  val (expr, rest) = tokens match
    case Token(op: TermOp, _, _) :: tailWithOperand =>
      val (operand, tail) = parseFactor(tailWithOperand)
      (Unary(op, operand), tail)
    case tailWithOperand =>
      parseFactor(tailWithOperand)
  _parseTerm(expr, rest)

def _parseTerm(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(op: TermOp, _, _) :: tailWithOperand =>
      val (right, rest) = parseFactor(tailWithOperand)
      _parseTerm(Binary(left, op, right), rest)
    case rest =>
      (left, rest)


type FactorOp = Star.type | Slash.type

def parseFactor(tokens: List[Token]): (Expr, List[Token]) =
  val (leftOperand, tail) = parsePower(tokens)
  _parseFactor(leftOperand, tail)

def _parseFactor(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(op: FactorOp, _, _) :: tailWithOperand =>
      val (right, rest) = parsePower(tailWithOperand)
      _parseFactor(Binary(left, op, right), rest)
    case rest =>
      (left, rest)


def parsePower(tokens: List[Token]): (Expr, List[Token]) =
  val (base, tail) = parsePrimary(tokens)
  tail match
    case Token(Caret, _, _) :: tailWithExponent =>
      val (exp, rest) = parsePower(tailWithExponent)
      (Binary(base, Caret, exp), rest)
    case rest =>
      (base, rest)


def parsePrimary(tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(Identifier, lexeme, _) :: rest =>
      (Name(lexeme), rest)
    case Token(Num, lexeme, _) :: rest =>
      (Const(lexeme.toDouble), rest)
    case Token(True, _, _) :: rest =>
      (Const(true), rest)
    case Token(False, _, _) :: rest =>
      (Const(false), rest)
    case Token(LParen, _, _) :: tailWithExpresion =>
      val (expr, rest) = parseExpression(tailWithExpresion)
      (expr, rest.consume(RParen))
    case _ => ???


extension (tokens: List[Token])
  def consume(typ: TokenType): List[Token] =
    if tokens.head.typ == typ
      then tokens.tail
      else ???


// AST
// ===


sealed trait AST

sealed trait Stmt extends AST
case class LetStmt(target: Name, value: Expr) extends Stmt

sealed trait Expr extends Stmt
case class IfExpr(condition: Expr, thenBranch: Expr, elseBranch: Option[Expr] = None) extends Expr
case class Block(stmts: List[Stmt], expr: Option[Expr] = None) extends Expr
case class LazyBinary(left: Expr, op: LazyOp, right: Expr) extends Expr
case class Binary(left: Expr, op: InfixOp, right: Expr) extends Expr
case class Unary(op: PrefixOp, operand: Expr) extends Expr
case class Const(value: YaslValue) extends Expr
case class Name(value: String) extends Expr

type LazyOp = And.type | Or.type
type InfixOp = TermOp | FactorOp | Caret.type | ComparisonOp
type PrefixOp = TermOp | Not.type


// Lexer
// =====


sealed trait LexemeType

export TokenType.*
enum TokenType extends LexemeType:
  case Num
  case Identifier

  case Plus, Minus, Star, Slash, Caret
  case EqEqual, NotEqual, Less, Greater, LessEqual, GreaterEqual
  case LParen, RParen, LBrace, RBrace
  case Equal
  case Colon

  case And, Or, Not
  case True, False

  case Let
  case If, Else

export WhiteLexeme.*
enum WhiteLexeme extends LexemeType:
  case WhiteSpace
  case Comment


val keywords = Map[String, TokenType](
  "let" -> Let,
  "true" -> True,
  "false" -> False,
  "and" -> And,
  "or" -> Or,
  "not" -> Not,
  "if" -> If,
  "else" -> Else,
)


val lexemePatterns = List[(Regex, LexemeType)](
  "\\s+".r -> WhiteSpace,
  "#.*".r -> Comment,
  raw"\d+(\.\d+)?".r -> Num,
  raw"[\w&&[^\d]]\w*".r -> Identifier,
  "\\+".r -> Plus,
  "-".r -> Minus,
  "\\*".r -> Star,
  "/".r -> Slash,
  "\\^".r -> Caret,
  "\\(".r -> LParen,
  "\\)".r -> RParen,
  "\\{".r -> LBrace,
  "\\}".r -> RBrace,
  ";".r -> Colon,
  "==".r -> EqEqual,
  "!=".r -> NotEqual,
  "<=".r -> LessEqual,
  ">=".r -> GreaterEqual,
  "<".r -> Less,
  ">".r -> Greater,
  "=".r -> Equal,
)


case class Token(typ: TokenType, lexeme: String, location: LocationRange)


def scanTokens(fileText: String, fileName: String = "<string>"): Either[TokenizeException, List[Token]] =
  var currentLocation = Location.beginingOf(fileText, fileName)
  val tokens = collection.mutable.ListBuffer.empty[Token]
  boundary:
    while !currentLocation.atEnd do
      matchLexeme(currentLocation, lexemePatterns) match
        case Right((_: WhiteLexeme, _, locationRange)) =>
          currentLocation = locationRange.stop
        case Right((typ: TokenType, lexeme, locationRange)) =>
          currentLocation = locationRange.stop
          tokens += Token(keywords.getOrElse(lexeme, typ), lexeme, locationRange)
        case Left(exc) => break(Left(exc))
    Right(tokens.toList)


def matchLexeme(
  location: Location, lexemePatterns: List[(Regex, LexemeType)]
): Either[TokenizeException, (LexemeType, String, LocationRange)] =
  boundary:
    for (pattern, typ) <- lexemePatterns do
      pattern.findPrefixOf(location.restOfText).foreach: lexeme =>
        val start = location
        val stop = location.moveBy(lexeme.length)
        break:
          Right((typ, lexeme, LocationRange(start, stop)))

    Left(TokenizeException(location))


trait ScanningException

case class TokenizeException(location: Location)
  extends Exception(s"${location.fileName}:${location.lnum}:${location.col}")



case class SourceCode(text: String, name: String)

case class LocationRange(start: Location, stop: Location):
  assert(start.fileText == stop.fileText)
  assert(start.fileName == stop.fileName)

  def fileText: String = start.fileText
  def fileName: String = start.fileName


object Location:
  def beginingOf(fileText: String, fileName: String = "<string>"): Location =
    Location(idx=0, lnum=1, col=1, fileText, fileName)

case class Location(idx: Int, lnum: Int, col: Int, fileText: String, fileName: String):

  def atEnd: Boolean = idx >= fileText.length
  def char: Char = fileText(idx)

  def moveBy(steps: Int): Location =
    var (_idx, _lnum, _col) = (idx, lnum, col)
    for _ <- (1 to steps) do
      if char == '\n' then
        _idx += 1
        _lnum += 1
        _col = 1
      else
        _idx += 1
        _col += 1
    Location(_idx, _lnum, _col, fileText, fileName)

  def restOfText: String = fileText.substring(idx)
