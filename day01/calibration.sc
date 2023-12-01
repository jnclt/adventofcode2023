val lines = io.Source.fromFile("input.txt").getLines().toList

def fromDigits(from: String): Int =
  val left = from.find(_.isDigit).get
  val right = from.findLast(_.isDigit).get
  s"${left}${right}".toInt

val numerals = Set(
  ("one", 1),
  ("two", 2),
  ("three", 3),
  ("four", 4),
  ("five", 5),
  ("six", 6),
  ("seven", 7),
  ("eight", 8),
  ("nine", 9),
  ("1", 1),
  ("2", 2),
  ("3", 3),
  ("4", 4),
  ("5", 5),
  ("6", 6),
  ("7", 7),
  ("8", 8),
  ("9", 9)
)

def fromNumerals(from: String): Int =
  val left = numerals.minBy(n =>
    from.indexOf(n._1) match
      case -1 => Int.MaxValue
      case i  => i
  )
  val right = numerals.maxBy(n => from.lastIndexOf(n._1))
  (left._2 * 10) + right._2

println(lines.map(fromDigits).sum)
println(lines.map(fromNumerals).sum)
