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

  case Unary(op, operand) => eval(operand, env) match
    case value: Double => op match
      case Plus => value
      case Minus => -value
    case YaslNil => ???

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

  case Block(stmts, expr) =>
    stmts.foreach(exec(_, env))
    expr.map(eval(_, env)).getOrElse(YaslNil)


type Environment = collection.mutable.Map[String, YaslValue]
object Environment:
  def apply(items: (String, YaslValue)*): Environment =
    collection.mutable.Map[String, YaslValue](items*)


type YaslValue = Double | Boolean | YaslNil.type

object YaslNil


// Parser
// ======

/*
 * block        ::= statement* expression?
 *
 * statement    ::= "let" IDENTIFIER "=" expression ";"
 *                | expression ";"
 *
 * expression   ::= comparison
 * comparison   ::= term ((">"|">="|"<"|"<="|"!="|"==") term)?
 * term         ::= ("+"|"-")? factor | term ("+"|"-") factor
 * factor       ::= power | factor ("*"|"/") power
 * power        ::= primary ("^" power)?
 * primary      ::= IDENTIFIER | NUMBER | "(" expression ")"
 */


def parse(tokens: List[Token]): Block =
  val (ast, rest) = parseBlock(tokens)
  assert(rest == Nil)
  ast


def parseBlock(tokens: List[Token]): (Block, List[Token]) =
  val stmts = collection.mutable.ListBuffer.empty[Stmt]
  var expr: Option[Expr] = None
  def go(tokens: List[Token]): List[Token] =
    Try(parseStatement(tokens)) match
      case Success(stmt, rest) =>
        stmts += stmt
        go(rest)
      case Failure(_) =>
        Try(parseExpression(tokens)) match
          case Success(expr_, rest) =>
            expr = Some(expr_)
            rest
          case Failure(_) =>
            tokens
  val rest = go(tokens)
  (Block(stmts.toList, expr), rest)

def _parseBlock(tokens: List[Token]): Block =
  val stmts = collection.mutable.ListBuffer.empty[Stmt]
  def go(tokens: List[Token]): Unit =
    if tokens.nonEmpty then
      val (stmt, rest) = parseStatement(tokens)
      stmts += stmt
      go(rest)
  go(tokens)
  Block(stmts.toList)


def parseStatement(tokens: List[Token]): (Stmt, List[Token]) = tokens match
  case Token(Let, _, _) :: Token(Identifier, target, _) :: Token(Equal, _, _) :: tailWithValue =>
    val (expr, tailWithColon) = parseExpression(tailWithValue)
    tailWithColon match
      case Token(Colon, _, _) :: rest =>
        (LetStmt(Name(target), expr), rest)
      case _ => ???
  case tokens =>
    val (expr, rest) = parseExpression(tokens)
    rest match
      case Token(Colon, _, _) :: rest => (expr, rest)
      case _ => ???


def parseExpression(tokens: List[Token]): (Expr, List[Token]) =
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
    case Token(LParen, _, _) :: tailWithExpresion =>
      val (expr, tailWithCloseParen) = parseExpression(tailWithExpresion)
      tailWithCloseParen match
        case Token(RParen, _, _) :: rest =>
          (expr, rest)
        case _ => ???
    case _ => ???


// AST
// ===


sealed trait AST

sealed trait Stmt extends AST
case class LetStmt(target: Name, value: Expr) extends Stmt

sealed trait Expr extends Stmt
case class Block(stmts: List[Stmt], expr: Option[Expr] = None) extends Expr
case class Binary(left: Expr, op: InfixOp, right: Expr) extends Expr
case class Unary(op: PrefixOp, operand: Expr) extends Expr
case class Const(value: YaslValue) extends Expr
case class Name(value: String) extends Expr

type InfixOp = TermOp | FactorOp | Caret.type | ComparisonOp
type PrefixOp = TermOp


// Lexer
// =====


sealed trait LexemeType

export TokenType.*
enum TokenType extends LexemeType:
  case Num
  case Identifier

  case Plus, Minus, Star, Slash, Caret
  case EqEqual, NotEqual, Less, Greater, LessEqual, GreaterEqual
  case LParen, RParen
  case Equal
  case Colon

  case Let

export WhiteLexeme.*
enum WhiteLexeme extends LexemeType:
  case WhiteSpace
  case Comment


val keywords = Map[String, TokenType](
  "let" -> Let,
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
