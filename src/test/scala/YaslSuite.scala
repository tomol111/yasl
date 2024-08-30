package yasl

import org.scalatest.flatspec.AnyFlatSpec


def disassembleToken(token: Token): (TokenType, String) = (token.typ, token.lexeme)


class LexerSpec extends AnyFlatSpec:
  behavior of "Lexer"

  it should "parse one character operators" in:
    val result = scanTokens("+-*/^()=").getOrElse(null).map(disassembleToken)
    val expected = List(
      Plus -> "+",
      Minus -> "-",
      Star -> "*",
      Slash -> "/",
      Caret -> "^",
      LParen -> "(",
      RParen -> ")",
      Assign -> "=",
    )
    assert(result == expected)


  it should "parse numbers" in:
    val result = scanTokens("123").getOrElse(null).map(disassembleToken)
    val expected = List(
      Num -> "123",
    )
    assert(result == expected)


  it should "parse floting point numbers" in:
    val result = scanTokens("123.456").getOrElse(null).map(disassembleToken)
    val expected = List(
      Num -> "123.456",
    )
    assert(result == expected)


  it should "parse names" in:
    val result = scanTokens("a_3").getOrElse(null).map(disassembleToken)
    val expected = List(
      Name -> "a_3",
    )
    assert(result == expected)


  it should "ignore white spaces" in:
    val result = scanTokens("\tnumber *\n123").getOrElse(null).map(disassembleToken)
    val expected = List(
      Name -> "number",
      Star -> "*",
      Num -> "123",
    )
    assert(result == expected)


  it should "specify token location" in:
    val code = " number *\n123"
    val result = scanTokens(code, "FILE_NAME").getOrElse(null)
    val expected = List(
      Token(
        Name,
        "number",
        LocationRange(
          start=Location(idx=1, lnum=1, col=2, code, "FILE_NAME"),
          stop=Location(idx=7, lnum=1, col=8, code, "FILE_NAME"),
        )
      ),
      Token(
        Star,
        "*",
        LocationRange(
          start=Location(idx=8, lnum=1, col=9, code, "FILE_NAME"),
          stop=Location(idx=9, lnum=1, col=10, code, "FILE_NAME"),
        )
      ),
      Token(
        Num,
        "123",
        LocationRange(
          start=Location(idx=10, lnum=2, col=1, code, "FILE_NAME"),
          stop=Location(idx=13, lnum=2, col=4, code, "FILE_NAME"),
        )
      ),
    )
    assert(result == expected)


  it should "detect keywords" in:
    val result = scanTokens("val abc").getOrElse(null).map(disassembleToken)
    val expected = List(
      Val -> "val",
      Name -> "abc"
    )
    assert(result == expected)


  it should "give information where tokenizning failed" in:
    val code = "10 ~ 20"
    val result = scanTokens(code, "FILE_NAME").left.getOrElse(null)
    val exception = TokenizeException(Location(3, 1, 4, code, "FILE_NAME"))
    assert(result == exception)


  it should "ignore single line comments" in:
    val code =
      """
        |# comment 1
        |abcd
        |  # comment 2
      """.stripMargin
    val result = scanTokens(code).getOrElse(null).map(disassembleToken)
    val expected = List(Name -> "abcd")
    assert(result == expected)


class ParserSpec extends AnyFlatSpec:
  behavior of "Parser - parsing expresions"

  it should "parse number literal" in:
    val (expr, rest) = expression(scanTokens("234").getOrElse(null))
    assert(expr == Const(234))
    assert(rest == Nil)

  it should "parse factor" in:
    val (expr, rest) = expression(scanTokens("3 * 10 / 20").getOrElse(null))
    assert(expr == BinOp(BinOp(Const(3), Star, Const(10)), Slash, Const(20)))
    assert(rest == Nil)

  it should "parse term" in:
    val (expr, rest) = expression(scanTokens("-2 + 10 - 3").getOrElse(null))
    assert(
      expr ==
      BinOp(
        BinOp(UnaryOp(Minus, Const(2)), Plus, Const(10)),
        Minus,
        Const(3)
      )
    )
    assert(rest == Nil)

  it should "parse parenthesized expression" in:
    val (expr, rest) = expression(scanTokens("12 - (10 - 3)").getOrElse(null))
    assert(expr == BinOp(Const(12), Minus, BinOp(Const(10), Minus, Const(3))))
    assert(rest == Nil)

  it should "parse exponent expression" in:
    val (expr, rest) = expression(scanTokens("2^10^3").getOrElse(null))
    assert(expr == BinOp(Const(2), Caret, BinOp(Const(10), Caret, Const(3))))
    assert(rest == Nil)


class InterpreterSuite extends AnyFlatSpec:
  behavior of "Interpreter"

  it should "eval constant" in:
    assert(eval(Const(10)) == 10)

  it should "eval unary operation" in:
    assert(eval(UnaryOp(Plus, Const(12))) == 12)
    assert(eval(UnaryOp(Minus, Const(10))) == -10)

  it should "eval binary operation" in:
    assert(eval(BinOp(Const(3), Plus, Const(12))) == 15)
    assert(eval(BinOp(Const(4), Minus, Const(1))) == 3)
    assert(eval(BinOp(Const(3), Star, Const(10))) == 30)
    assert(eval(BinOp(Const(20), Slash, Const(5))) == 4)
    assert(eval(BinOp(Const(2), Caret, Const(3))) == 8)
