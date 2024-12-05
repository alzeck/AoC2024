//> using toolkit 0.6.0
import scala.annotation.tailrec

def validUpdate(
    orderingRules: Map[Int, List[Int]],
    update: List[Int]
): Boolean =
  update match
    case head :: tails
        if orderingRules.contains(head) &&
          !tails.filter(orderingRules(head).contains(_)).isEmpty =>
      false
    case _ :: tails => validUpdate(orderingRules, tails)
    case _          => true

def sortWithRules(orderingRules: Map[Int, List[Int]]) =
  (a: Int, b: Int) =>
    orderingRules.get(b) match
      case Some(b) if b.contains(a) => false
      case _                        => true

@main def main() =
  val path = os.pwd / "input.txt";
  val content = os.read(path).split("\n").toList;

  val orderingRules = content
    .takeWhile(_ != "")
    .map(
      _.split('|')
        .map(_.toInt)
        .toList
    )
    .groupBy(_.head)
    .view
    .mapValues(_.map(_.tail).flatten)
    .toMap

  val updates =
    content
      .dropWhile(_ != "")
      .tail
      .map(
        _.split(',')
          .map(_.toInt)
          .toList
      )

  val result1 = updates
    .filter(a => validUpdate(orderingRules, a.reverse))
    .map(a => a(a.length / 2))
    .sum

  val result2 =
    updates
      .filter(a => !validUpdate(orderingRules, a.reverse))
      .map(_.sortWith(sortWithRules(orderingRules)))
      .map(a => a(a.length / 2))
      .sum

  println(s"Result Pt. 1: $result1")
  println(s"Result Pt. 2: $result2")
