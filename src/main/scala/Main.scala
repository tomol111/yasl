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
  print("yasl> ")
  io.StdIn.readLine() match
    case null =>
      println()
    case text =>
      run(text, "<REPR>")
      runPrompt()


def runFile(fileName: String): Unit =
  val filePath = os.Path(os.FilePath(fileName), os.pwd)
  val fileText = os.read(filePath)
  run(fileText, filePath.toString)


def run(code: String, name: String) =
  scanTokens(code, name)
    .left.map(println(_)).toOption
    .map(parse)
    .map(dumpAST(_))


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

    case BinOp(left, op, right) =>
      println("BinOp")

      print(s"${deeperIndent}left: ")
      dumpAST(left, deeper)

      println(s"${deeperIndent}op: $op")

      print(s"${deeperIndent}right: ")
      dumpAST(right, deeper)

    case UnaryOp(op, operand) =>
      println("UnaryOP")

      println(s"${deeperIndent}op: $op")

      print(s"${deeperIndent}operand: ")
      dumpAST(operand, deeper)
