//> using toolkit 0.6.0
import scala.annotation.tailrec

enum Direction:
  case UpLeft, UpRight, DownLeft, DownRight, Left, Right, Up, Down

def nextLetter(curr: Char): Option[Char] =
  curr match
    case 'X' => Some('M')
    case 'M' => Some('A')
    case 'A' => Some('S')
    case 'S' => None
    case _   => None

def isOutOfBounds(cw: List[List[Char]], i: Int, j: Int) =
  i < 0 || i >= cw.length || j < 0 || j >= cw(i).length

def checkSpot(cw: List[List[Char]], i: Int, j: Int, letter: Char) =
  !isOutOfBounds(cw, i, j) && cw(i)(j) == letter

def nextSpot(
    i: Int,
    j: Int,
    dir: Direction
): (Int, Int) =
  dir match
    case Direction.UpLeft    => (i - 1, j - 1)
    case Direction.UpRight   => (i - 1, j + 1)
    case Direction.DownLeft  => (i + 1, j - 1)
    case Direction.DownRight => (i + 1, j + 1)
    case Direction.Up        => (i - 1, j)
    case Direction.Down      => (i + 1, j)
    case Direction.Left      => (i, j - 1)
    case Direction.Right     => (i, j + 1)

@tailrec
def checkDirection(
    cw: List[List[Char]],
    i: Int,
    j: Int,
    dir: Direction,
    letter: Char = 'X'
): Int =
  letter match
    case 'S' if checkSpot(cw, i, j, letter) => 1
    case _ if !checkSpot(cw, i, j, letter)  => 0
    case _ =>
      nextLetter(letter) match
        case None => 0
        case Some(nextLetter) =>
          val (ni, nj) = nextSpot(i, j, dir)
          checkDirection(cw, ni, nj, dir, nextLetter)

def checkWord(cw: List[List[Char]], i: Int, j: Int): Int =
  checkDirection(cw, i, j, Direction.UpLeft) +
    checkDirection(cw, i, j, Direction.UpRight) +
    checkDirection(cw, i, j, Direction.DownLeft) +
    checkDirection(cw, i, j, Direction.DownRight) +
    checkDirection(cw, i, j, Direction.Up) +
    checkDirection(cw, i, j, Direction.Down) +
    checkDirection(cw, i, j, Direction.Left) +
    checkDirection(cw, i, j, Direction.Right)

def checkXmas(cw: List[List[Char]], i: Int, j: Int): Int =
  if checkSpot(cw, i, j, 'A')
    && (checkDirection(cw, i + 1, j + 1, Direction.UpLeft,'M') +
      checkDirection(cw, i + 1, j - 1, Direction.UpRight,'M') +
      checkDirection(cw, i - 1, j + 1, Direction.DownLeft,'M') +
      checkDirection(cw, i - 1, j - 1, Direction.DownRight,'M')) == 2
  then 1
  else 0

@tailrec
def walkCrossword(
    check: (List[List[Char]], Int, Int) => Int,
    cw: List[List[Char]],
    i: Int = 0,
    j: Int = 0,
    count: Int = 0
): Int =
  if (i >= 0 && i < cw.length) then
    if (j >= 0 && j < cw(i).length) then
      walkCrossword(check, cw, i, j + 1, count + check(cw, i, j))
    else walkCrossword(check, cw, i + 1, 0, count + check(cw, i, j))
  else count

@main def main() =
  val path = os.pwd / "input.txt";
  val content = os.read(path).split("\n").toList.map(_.toList);

  println("Result Pt. 1: " + walkCrossword(checkWord, content).toString)
  println("Result Pt. 2: " + walkCrossword(checkXmas, content).toString)
