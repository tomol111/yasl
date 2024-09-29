package yasl

import util.boundary, boundary.break


@main def main(args: String*): Unit =
  args match
    case Nil =>
      runPrompt()
    case fileName :: Nil =>
      runFile(fileName)
    case _ =>
      println("Usage: jasl [script]")
      sys.exit(64)


def runPrompt(): Unit =
  println("YASL")
  val env = Environment(
    "println" -> yaslPrintln,
  )
  def step(): Unit =
    print("> ")
    io.StdIn.readLine() match
      case null =>  // EOF
        println()  // add missing '\n'
      case input =>
        scanTokens(input, "<REPL>")
          .left.map(println(_)).toOption
          .map(parse)
          //.map(dumpAST(_))
          .map(eval(_, env))
          .foreach:
            case YaslNil => ()
            case value => println(value)
        step()
  step()


def runFile(fileName: String): Unit =
  val filePath = os.Path(os.FilePath(fileName), os.pwd)
  val fileText = os.read(filePath)
  val env = Environment()
  scanTokens(fileText, fileName)
    .left.map(println(_)).toOption
    .map(parse)
    .map(exec(_, env))


def dumpTokens(tokens: Iterable[Token]): Unit =
  for token <- tokens do
    val Location(_, lnumStart, colStart, _, _) = token.location.start
    val Location(_, lnumStop, colStop, _, _) = token.location.stop
    println(s"$lnumStart:$colStart-$lnumStop:$colStop ${token.typ} \"${token.lexeme}\"")


def dumpAST(tree: AST, depth: Int = 0): Unit =
  val deeper = depth + 1
  val deeperIndent = " " * 2 * deeper
  tree match
    case Const(const) =>
      println(s"Const $const")

    case Name(name) =>
      println(s"Name $name")

    case Binary(left, op, right) =>
      println("Binary")

      print(s"${deeperIndent}left: ")
      dumpAST(left, deeper)

      println(s"${deeperIndent}op: $op")

      print(s"${deeperIndent}right: ")
      dumpAST(right, deeper)

    case Unary(op, operand) =>
      println("Unary")

      println(s"${deeperIndent}op: $op")

      print(s"${deeperIndent}operand: ")
      dumpAST(operand, deeper)

    case LetStmt(target, value) =>
      println("LetStmt")

      print(s"${deeperIndent}target: ")
      dumpAST(target, deeper)

      print(s"${deeperIndent}value: ")
      dumpAST(value, deeper)

    case Block(stmts, expr) =>
      println("Block")

      for (stmt, i) <- stmts.zipWithIndex do
        print(s"${deeperIndent}$i: ")
        dumpAST(stmt, deeper)

      expr.foreach { exp =>
        print(s"${deeperIndent}expr: ")
        dumpAST(exp, deeper)
      }
