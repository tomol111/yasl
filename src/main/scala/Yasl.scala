package yasl

import util.boundary, boundary.break
import util.matching.Regex


// Interpreter
// ===========


def eval(expr: Expr): Double = expr match
  case Const(value) => value

  case UnaryOp(operator, operand) => operator match
    case Plus => eval(operand)
    case Minus => -eval(operand)

  case BinOp(left, op, right) => op match
    case Plus => eval(left) + eval(right)
    case Minus => eval(left) - eval(right)
    case Star => eval(left) * eval(right)
    case Slash => eval(left) / eval(right)
    case Caret => math.pow(eval(left), eval(right))


// Parser
// ======

/*
 * expression   ::= term
 * term         ::= ("+"|"-")? factor | term ("+"|"-") factor
 * factor       ::= power | factor ("*"|"/") power
 * power        ::= primary ("^" power)?
 * primary      ::= "(" expression ")" | NUMBER
 */

// comparison   ::= term ((">"|">="|"<"|"<="|"!="|"==") term)*

def parse(tokens: List[Token]): AST =
  val (expr, rest) = expression(tokens)
  assert(rest.isEmpty)
  expr

def expression(tokens: List[Token]): (Expr, List[Token]) =
  term(tokens)

type TermOp = Plus.type | Minus.type

def term(tokens: List[Token]): (Expr, List[Token]) =
  val (expr, rest) = tokens match
    case Token(op: TermOp, _, _) :: tailWithOperand =>
      val (operand, tail) = factor(tailWithOperand)
      (UnaryOp(op, operand), tail)
    case tailWithOperand =>
      factor(tailWithOperand)
  _term(expr, rest)

def _term(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(op: TermOp, _, _) :: tailWithOperand =>
      val (right, rest) = factor(tailWithOperand)
      _term(BinOp(left, op, right), rest)
    case rest =>
      (left, rest)

type FactorOp = Star.type | Slash.type

def factor(tokens: List[Token]): (Expr, List[Token]) =
  val (leftOperand, tail) = power(tokens)
  _factor(leftOperand, tail)

def _factor(left: Expr, tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(op: FactorOp, _, _) :: tailWithOperand =>
      val (right, rest) = power(tailWithOperand)
      _factor(BinOp(left, op, right), rest)
    case rest =>
      (left, rest)

def power(tokens: List[Token]): (Expr, List[Token]) =
  val (base, tail) = primary(tokens)
  tail match
    case Token(Caret, _, _) :: tailWithExponent =>
      val (exp, rest) = power(tailWithExponent)
      (BinOp(base, Caret, exp), rest)
    case rest =>
      (base, rest)

def primary(tokens: List[Token]): (Expr, List[Token]) =
  tokens match
    case Token(LParen, _, _) :: tailWithExpresion =>
      val (expr, tailWithCloseParen) = expression(tailWithExpresion)
      tailWithCloseParen match
        case Token(RParen, _, _) :: rest =>
          (expr, rest)
        case _ => ???
    case Token(Num, lexeme, _) :: rest =>
      (Const(lexeme.toDouble), rest)
    case _ => ???


sealed trait AST

sealed trait Expr extends AST

case class BinOp(left: Expr, op: InfixOp, right: Expr) extends Expr
case class UnaryOp(op: PrefixOp, operand: Expr) extends Expr
case class Const(value: Double) extends Expr

type InfixOp = Plus.type | Minus.type | Star.type | Slash.type | Caret.type
type PrefixOp = Plus.type | Minus.type


// Lexer
// =====


sealed trait LexemeType

export TokenType.*
enum TokenType extends LexemeType:
  case Num
  case Name

  case Plus, Minus, Star, Slash, Caret
  case LParen, RParen
  case Assign

  case Val

export WhiteLexeme.*
enum WhiteLexeme extends LexemeType:
  case WhiteSpace
  case Comment


val keywords = Map[String, TokenType](
  "val" -> Val,
)


val lexemePatterns = List[(Regex, LexemeType)](
  "\\s+".r -> WhiteSpace,
  "#.*".r -> Comment,
  raw"\d+(\.\d+)?".r -> Num,
  raw"[\w&&[^\d]]\w*".r -> Name,
  "\\+".r -> Plus,
  "-".r -> Minus,
  "\\*".r -> Star,
  "/".r -> Slash,
  "\\^".r -> Caret,
  "\\(".r -> LParen,
  "\\)".r -> RParen,
  "=".r -> Assign,
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
