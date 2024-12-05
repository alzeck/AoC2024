//> using toolkit 0.6.0
import scala.annotation.tailrec

enum Token:
  case Number(value: Int);
  case Mul, Comma, OpenBrackets, CloseBracket, InvalidToken, Dont, Do;

def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
def isCloseBracket(c: Char): Boolean = c == ')'
def isOpenBracket(c: Char): Boolean = c == '('
def isComma(c: Char): Boolean = c == ','
def isMul(m: List[Char]): Boolean = m.take(3).mkString == "mul"
def isDo(m: List[Char]): Boolean = m.take(4).mkString == "do()"
def isDont(m: List[Char]): Boolean = m.take(7).mkString == "don't()"

@tailrec
def tokenizer(
    lis: List[Char],
    parsed: List[Token] = List(),
    invalidToken: Boolean = false
): List[Token] =
  lis match
    case Nil => parsed
    case head :: tail if isDigit(head) => {
      val (digits, rest) = lis.span(isDigit)
      tokenizer(rest, Token.Number(digits.mkString.toInt) :: parsed)
    }
    case lis if isMul(lis) =>
      tokenizer(lis.drop(3), Token.Mul :: parsed)
    case lis if isDo(lis) =>
      tokenizer(lis.drop(4), Token.Do :: parsed)
    case lis if isDont(lis) =>
      tokenizer(lis.drop(7), Token.Dont :: parsed)

    case c :: tail if isComma(c) =>
      tokenizer(tail, Token.Comma :: parsed)

    case c :: tail if isOpenBracket(c) =>
      tokenizer(tail, Token.OpenBrackets :: parsed)

    case c :: tail if isCloseBracket(c) =>
      tokenizer(tail, Token.CloseBracket :: parsed)

    case c :: tail if invalidToken => tokenizer(tail, parsed, true)
    case c :: tail => tokenizer(tail, Token.InvalidToken :: parsed, true)

@tailrec
def parse(
    tokens: List[Token],
    acc: Int = 0
): Int = tokens match
  case Token.Mul :: Token.OpenBrackets
      :: Token.Number(a) :: Token.Comma :: Token.Number(b)
      :: Token.CloseBracket :: tail =>
    parse(tail, a * b + acc)
  case c :: tail => parse(tail, acc)
  case nil       => acc

@tailrec
def parseDo(
    tokens: List[Token],
    acc: Int = 0,
    enabled: Boolean = true
): Int = tokens match
  case Token.Mul :: Token.OpenBrackets
      :: Token.Number(a) :: Token.Comma :: Token.Number(b)
      :: Token.CloseBracket :: tail if enabled =>
    parseDo(tail, a * b + acc)
  case Token.Do :: tail   => parseDo(tail, acc, true)
  case Token.Dont :: tail => parseDo(tail, acc, false)
  case c :: tail          => parseDo(tail, acc, enabled)
  case nil                => acc

@tailrec
def printTokens(tokens: List[Token]):Unit =
  tokens match
    case Nil => println("")
    case head :: tail  => 
      println(head)
      printTokens(tail)

@main def main() =
  val path = os.pwd / "input.txt";
  val content = os.read(path).toList;
  val tokens = tokenizer(content).reverse;

  println("result 1: " ++ parse(tokens).toString());
  println("result 2: " ++ parseDo(tokens).toString());
