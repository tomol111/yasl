package yasl

import org.scalatest.flatspec.AnyFlatSpec


def disassembleToken(token: Token): (TokenType, String) = (token.typ, token.lexeme)


class LexerSpec extends AnyFlatSpec:
  behavior of "Lexer"

  it should "parse one character operators" in:
    val result = scanTokens("+-*/^()=;").toOption.get.map(disassembleToken)
    val expected = List(
      Plus -> "+",
      Minus -> "-",
      Star -> "*",
      Slash -> "/",
      Caret -> "^",
      LParen -> "(",
      RParen -> ")",
      Equal -> "=",
      Colon -> ";",
    )
    assert(result == expected)


  it should "parse numbers" in:
    val result = scanTokens("123").toOption.get.map(disassembleToken)
    val expected = List(
      Num -> "123",
    )
    assert(result == expected)


  it should "parse floting point numbers" in:
    val result = scanTokens("123.456").toOption.get.map(disassembleToken)
    val expected = List(
      Num -> "123.456",
    )
    assert(result == expected)


  it should "parse names" in:
    val result = scanTokens("a_3").toOption.get.map(disassembleToken)
    val expected = List(
      Identifier -> "a_3",
    )
    assert(result == expected)


  it should "ignore white spaces" in:
    val result = scanTokens("\tnumber *\n123").toOption.get.map(disassembleToken)
    val expected = List(
      Identifier -> "number",
      Star -> "*",
      Num -> "123",
    )
    assert(result == expected)


  it should "specify token location" in:
    val code = " number *\n123"
    val result = scanTokens(code, "FILE_NAME").toOption.get
    val expected = List(
      Token(
        Identifier,
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
    val result = scanTokens("let abc").toOption.get.map(disassembleToken)
    val expected = List(
      Let -> "let",
      Identifier -> "abc"
    )
    assert(result == expected)


  it should "give information where tokenizning failed" in:
    val code = "10 ~ 20"
    val result = scanTokens(code, "FILE_NAME").left.toOption.get
    val exception = TokenizeException(Location(3, 1, 4, code, "FILE_NAME"))
    assert(result == exception)


  it should "ignore single line comments" in:
    val code =
      """
        |# comment 1
        |abcd
        |  # comment 2
      """.stripMargin
    val result = scanTokens(code).toOption.get.map(disassembleToken)
    val expected = List(Identifier -> "abcd")
    assert(result == expected)


class ParserSpec extends AnyFlatSpec:
  behavior of "Parser - parsing expressions"

  it should "parse number literal" in:
    val (expr, rest) = parseExpression(scanTokens("234").toOption.get)
    assert(expr == Const(234))
    assert(rest == Nil)

  it should "parse name" in:
    val (expr, rest) = parseExpression(scanTokens("foo").toOption.get)
    assert(expr == Name("foo"))
    assert(rest == Nil)

  it should "parse factor" in:
    val (expr, rest) = parseExpression(scanTokens("3 * 10 / 20").toOption.get)
    assert(expr == Binary(Binary(Const(3), Star, Const(10)), Slash, Const(20)))
    assert(rest == Nil)

  it should "parse term" in:
    val (expr, rest) = parseExpression(scanTokens("-2 + 10 - 3").toOption.get)
    assert(
      expr ==
      Binary(
        Binary(Unary(Minus, Const(2)), Plus, Const(10)),
        Minus,
        Const(3)
      )
    )
    assert(rest == Nil)

  it should "parse parenthesized expression" in:
    val (expr, rest) = parseExpression(scanTokens("12 - (10 - 3)").toOption.get)
    assert(expr == Binary(Const(12), Minus, Binary(Const(10), Minus, Const(3))))
    assert(rest == Nil)

  it should "parse exponent expression" in:
    val (expr, rest) = parseExpression(scanTokens("2^10^3").toOption.get)
    assert(expr == Binary(Const(2), Caret, Binary(Const(10), Caret, Const(3))))
    assert(rest == Nil)


  behavior of "Parser - parsing statements"

  it should "parse expression statement" in:
    val (stat, rest) = parseStatement(scanTokens("5;").toOption.get)
    assert(stat == Const(5))
    assert(rest == Nil)

  it should "parse let statement" in:
    val (stat, rest) = parseStatement(scanTokens("let x = 10;").toOption.get)
    assert(stat == LetStmt(Name("x"), Const(10)))
    assert(rest == Nil)


  behavior of "Parser - parsing chunk"

  it should "parse chunk" in:
    val chunk = parseChunk(scanTokens("let x = 10; let y = x * 3;").toOption.get)
    assert:
      chunk == Chunk:
        List(
          LetStmt(Name("x"), Const(10)),
          LetStmt(Name("y"), Binary(Name("x"), Star, Const(3))),
        )


class InterpreterSuite extends AnyFlatSpec:
  behavior of "Interpreter - evaluate"

  it should "eval constant" in:
    assert(eval(Const(10), Environment()) == 10)

  it should "eval unary operation" in:
    assert(eval(Unary(Plus, Const(12)), Environment()) == 12)
    assert(eval(Unary(Minus, Const(10)), Environment()) == -10)

  it should "eval binary operation" in:
    assert(eval(Binary(Const(3), Plus, Const(12)), Environment()) == 15)
    assert(eval(Binary(Const(4), Minus, Const(1)), Environment()) == 3)
    assert(eval(Binary(Const(3), Star, Const(10)), Environment()) == 30)
    assert(eval(Binary(Const(20), Slash, Const(5)), Environment()) == 4)
    assert(eval(Binary(Const(2), Caret, Const(3)), Environment()) == 8)

  it should "evaluate variable to bounded value" in:
    val env = collection.mutable.Map[String, YaslValue]("x" -> 14)
    assert(eval(Unary(Minus, Name("x")), env) == -14)


  behavior of "Interpreter - execute"

  it should "add variable to the environment" in:
    val env = Environment()
    exec(LetStmt(Name("a"), Binary(Const(10), Plus, Const(2))), env)
    assert(env.get("a") == Some(12))

  it should "use variable from environment" in:
    val env = Environment("foo" -> 5)
    exec(LetStmt(Name("bar"), Binary(Name("foo"), Plus, Const(3))), env)
    assert(env.get("bar") == Some(8))

  it should "execute statement expresion" in:
    val env = Environment("foo" -> 5)
    exec(Binary(Const(9), Plus, Name("foo")), env)
    assert(env.toList == List("foo" -> 5))
