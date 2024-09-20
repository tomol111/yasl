package yasl

import org.scalatest.flatspec.AnyFlatSpec


def disassembleToken(token: Token): (TokenType, String) = (token.typ, token.lexeme)


class LexerSpec extends AnyFlatSpec:
  behavior of "Lexer"

  it should "parse operators and punctuations" in:
    val result = scanTokens("+ - * / ^ ( ) { } = ; , == != < > <= >=").toOption.get.map(disassembleToken)
    val expected = List(
      Plus -> "+",
      Minus -> "-",
      Star -> "*",
      Slash -> "/",
      Caret -> "^",
      LParen -> "(",
      RParen -> ")",
      LBrace -> "{",
      RBrace -> "}",
      Equal -> "=",
      Colon -> ";",
      Comma -> ",",
      EqEqual -> "==",
      NotEqual -> "!=",
      Less -> "<",
      Greater -> ">",
      LessEqual -> "<=",
      GreaterEqual -> ">=",
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
    val result = scanTokens("abc let true false and or not if else").toOption.get.map(disassembleToken)
    val expected = List(
      Identifier -> "abc",  // not keyword
      Let -> "let",
      True -> "true",
      False -> "false",
      And -> "and",
      Or -> "or",
      Not -> "not",
      If -> "if",
      Else -> "else",
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

  it should "parse `true` literal" in:
    val (expr, rest) = parseExpression(scanTokens("true").toOption.get)
    assert(expr == Const(true))
    assert(rest == Nil)

  it should "parse `false` literal" in:
    val (expr, rest) = parseExpression(scanTokens("false").toOption.get)
    assert(expr == Const(false))
    assert(rest == Nil)

  it should "parse parenthesized expression" in:
    val (expr, rest) = parseExpression(scanTokens("12 - (10 - 3)").toOption.get)
    assert(expr == Binary(Const(12), Minus, Binary(Const(10), Minus, Const(3))))
    assert(rest == Nil)

  it should "parse call with no arguments" in:
    val (expr, rest) = parseExpression(scanTokens("foo()").toOption.get)
    assert(expr == Call(Name("foo"), Nil))
    assert(rest == Nil)

  it should "parse call with arguments" in:
    val (expr, rest) = parseExpression(scanTokens("foo(x, -3, true)").toOption.get)
    assert:
      expr == Call(
        Name("foo"),
        List(Name("x"), Unary(Minus, Const(3)), Const(true))
      )
    assert(rest == Nil)

  it should "parse call with trailing comma" in:
    val (expr, rest) = parseExpression(scanTokens("foo(x,)").toOption.get)
    assert:
      expr == Call(
        Name("foo"),
        List(Name("x")),
      )
    assert(rest == Nil)

  it should "recursively parse call" in:
    val (expr, rest) = parseExpression(scanTokens("foo(1)(2)").toOption.get)
    assert:
      expr == Call(
        Call(Name("foo"), List(Const(1))),
        List(Const(2)),
      )
    assert(rest == Nil)

  it should "parse exponent expression" in:
    val (expr, rest) = parseExpression(scanTokens("2^10^3").toOption.get)
    assert(expr == Binary(Const(2), Caret, Binary(Const(10), Caret, Const(3))))
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

  it should "parse comparison" in:
    val (expr, rest) = parseExpression(scanTokens("4 + 2 != -10").toOption.get)
    assert(
      expr ==
      Binary(
        Binary(Const(4), Plus, Const(2)),
        NotEqual,
        Unary(Minus, Const(10))
      )
    )
    assert(rest == Nil)

  it should "parse negation" in:
    val (expr, rest) = parseExpression(scanTokens("not 3 < 9").toOption.get)
    assert(expr == Unary(Not, Binary(Const(3), Less, Const(9))))
    assert(rest == Nil)

  it should "parse `and` expr" in:
    val (expr, rest) = parseExpression(scanTokens("not true and true and not false").toOption.get)
    assert:
      expr == LazyBinary(
        LazyBinary(
          Unary(Not, Const(true)),
          And,
          Const(true),
        ),
        And,
        Unary(Not, Const(false)),
      )
    assert(rest == Nil)

  it should "parse `or` expr" in:
    val (expr, rest) = parseExpression(scanTokens("true or false and true or not false").toOption.get)
    assert:
      expr == LazyBinary(
        LazyBinary(
          Const(true),
          Or,
          LazyBinary(Const(false), And, Const(true)),
        ),
        Or,
        Unary(Not, Const(false)),
      )
    assert(rest == Nil)

  it should "parse `if` expression" in:
    val (expr, rest) = parseExpression(scanTokens("if false or true {1}").toOption.get)
    assert:
      expr == IfExpr(
        LazyBinary(Const(false), Or, Const(true)),
        Block(Nil, Some(Const(1))),
      )
    assert(rest == Nil)

  it should "parse `if` expression with `else` branch" in:
    val (expr, rest) = parseExpression(scanTokens("if cond {1} else {2}").toOption.get)
    assert:
      expr == IfExpr(
        Name("cond"),
        Block(Nil, Some(Const(1))),
        Some(Block(Nil, Some(Const(2)))),
      )
    assert(rest == Nil)

  it should "chain `if` expressions" in:
    val input = """
      if cond {
          1
      } else if cond2 {
          2
      } else {
          3
      }
    """
    val (expr, rest) = parseExpression(scanTokens(input).toOption.get)
    assert:
      expr == IfExpr(
        Name("cond"),
        Block(Nil, Some(Const(1))),
        Some(
          IfExpr(
            Name("cond2"),
            Block(Nil, Some(Const(2))),
            Some(Block(Nil, Some(Const(3)))),
          )
        ),
      )
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


  behavior of "Parser - parsing block"

  it should "parse block content" in:
    val (block, rest) = parseBlockContent(scanTokens("let x = 10; let y = x * 3;").toOption.get)
    assert:
      block == Block:
        List(
          LetStmt(Name("x"), Const(10)),
          LetStmt(Name("y"), Binary(Name("x"), Star, Const(3))),
        )
    assert(rest == Nil)

  it should "parse block ending with expression" in:
    val (block, rest) = parseBlockContent(scanTokens("let x = 10; x * 3").toOption.get)
    assert:
      block == Block(
        List(LetStmt(Name("x"), Const(10))),
        Some(Binary(Name("x"), Star, Const(3))),
      )
    assert(rest == Nil)

  it should "parse block" in:
    val (block, rest) = parseBlock(scanTokens("{ let x = 10; x * 3 }").toOption.get)
    assert:
      block == Block(
        List(LetStmt(Name("x"), Const(10))),
        Some(Binary(Name("x"), Star, Const(3))),
      )
    assert(rest == Nil)


class InterpreterSuite extends AnyFlatSpec:
  behavior of "Interpreter - evaluate"

  it should "evalate constant" in:
    assert(eval(Const(10), Environment()) == 10.0)

  it should "evaluate variable to bounded value" in:
    val env = Environment("x" -> 14)
    assert(eval(Unary(Minus, Name("x")), env) == -14.0)

  it should "evaluate call" in:
    val callee = new YaslCallable:
      def call(args: List[YaslValue]): YaslValue =
        assert(args == List(1.0, true))
        10.0

    val env = Environment("foo" -> callee, "x" -> true)
    val result = eval(Call(Name("foo"), List(Const(1), Name("x"))), env)
    assert(result == 10.0)

  it should "evalate unary operation" in:
    assert(eval(Unary(Plus, Const(12)), Environment()) == 12.0)
    assert(eval(Unary(Minus, Const(10)), Environment()) == -10.0)
    assert(eval(Unary(Not, Const(true)), Environment()) == false)

  it should "evalate binary operation" in:
    assert(eval(Binary(Const(3), Plus, Const(12)), Environment()) == 15.0)
    assert(eval(Binary(Const(4), Minus, Const(1)), Environment()) == 3.0)
    assert(eval(Binary(Const(3), Star, Const(10)), Environment()) == 30.0)
    assert(eval(Binary(Const(20), Slash, Const(5)), Environment()) == 4.0)
    assert(eval(Binary(Const(2), Caret, Const(3)), Environment()) == 8.0)
    assert(eval(Binary(Const(2), EqEqual, Const(2)), Environment()) == true)
    assert(eval(Binary(Const(2), EqEqual, Const(3)), Environment()) == false)
    assert(eval(Binary(Const(2), NotEqual, Const(2)), Environment()) == false)
    assert(eval(Binary(Const(2), NotEqual, Const(3)), Environment()) == true)
    assert(eval(Binary(Const(2), Less, Const(9)), Environment()) == true)
    assert(eval(Binary(Const(9), Less, Const(9)), Environment()) == false)
    assert(eval(Binary(Const(9), Greater, Const(2)), Environment()) == true)
    assert(eval(Binary(Const(9), Greater, Const(9)), Environment()) == false)
    assert(eval(Binary(Const(2), LessEqual, Const(2)), Environment()) == true)
    assert(eval(Binary(Const(9), LessEqual, Const(2)), Environment()) == false)
    assert(eval(Binary(Const(2), GreaterEqual, Const(2)), Environment()) == true)
    assert(eval(Binary(Const(2), GreaterEqual, Const(9)), Environment()) == false)

  it should "evalate lazy binary operation" in:
    assert(eval(LazyBinary(Const(true), And, Const(true)), Environment()) == true)
    assert(eval(LazyBinary(Const(true), And, Const(false)), Environment()) == false)
    assert(eval(LazyBinary(Const(false), Or, Const(true)), Environment()) == true)
    assert(eval(LazyBinary(Const(false), Or, Const(false)), Environment()) == false)

  ignore should "lazily evalate lazy binary operation" in:
    ???  // language need side effects to test this

  it should "evaluate block with tail expression" in:
    val env = Environment()
    val block = Block(
      List(LetStmt(Name("x"), Const(10))),
      Some(Name("x")),
    )
    assert(eval(block, env) == 10.0)

  it should "evaluate block without tail expression" in:
    val env = Environment()
    val block = Block:
      List(
        LetStmt(Name("x"), Const(10)), LetStmt(Name("y"), Binary(Name("x"), Plus, Const(5)))
      )
    assert(eval(block, env) == YaslNil)

  it should "evaluate `if` expression" in:
    val env = Environment()
    val ifExpr = IfExpr(Name("cond"), Const(1), Some(Const(2)))
    env("cond") = true
    assert(eval(ifExpr, env) == 1.0)
    env("cond") = false
    assert(eval(ifExpr, env) == 2.0)

  it should "evaluate `if` expression even without `else` branch" in:
    val env = Environment()
    val ifExpr = IfExpr(Name("cond"), Const(1))
    env("cond") = true
    assert(eval(ifExpr, env) == 1.0)
    env("cond") = false
    assert(eval(ifExpr, env) == YaslNil)

  ignore should "lazily evaluate `if` expression" in:
    ???  // language need side effects to test this


  behavior of "Interpreter - execute"

  it should "add variable to the environment" in:
    val env = Environment()
    exec(LetStmt(Name("a"), Binary(Const(10), Plus, Const(2))), env)
    assert(env.get("a") == Some(12))

  it should "use variable from environment" in:
    val env = Environment("foo" -> 5)
    exec(LetStmt(Name("bar"), Binary(Name("foo"), Plus, Const(3))), env)
    assert(env.get("bar") == Some(8))

  it should "execute statement expression" in:
    val env = Environment("foo" -> 5)
    exec(Binary(Const(9), Plus, Name("foo")), env)
    assert(env.toList == List("foo" -> 5))
