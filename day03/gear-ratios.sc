#!/usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines.toVector
val (maxRow, maxCol) = (lines.size - 1, lines.head.size - 1)

def charEnvelope(x: Int, y: Int): Set[Char] =
  val above = if y > 0 then y - 1 else y
  val below = if y < maxRow then y + 1 else y
  val left = if x > 0 then x - 1 else x
  val right = if x < maxCol then x + 1 else x
  Set(
    lines(above)(x),
    lines(below)(x),
    lines(y)(left),
    lines(y)(right),
    lines(above)(left),
    lines(above)(right),
    lines(below)(left),
    lines(below)(right)
  )

def isSymbolInEnvelope(x: Int, y: Int): Boolean =
  !charEnvelope(x, y).forall(c => ('0' to '9').toSet + '.' contains c)

val number = """\d+""".r
def sumLine(line: String, y: Int): Int =
  number
    .findAllMatchIn(line)
    .map { m =>
      if (m.start to m.end - 1).exists(x => isSymbolInEnvelope(x, y)) then
        m.matched.toInt
      else 0
    }
    .sum

println(lines.zipWithIndex.map(sumLine).sum)
