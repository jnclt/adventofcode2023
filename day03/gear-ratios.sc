#!/usr/bin/env -S scala-cli shebang

import scala.collection.mutable.ArrayBuffer
val lines = io.Source.fromFile("input.txt").getLines.toVector
val (maxRow, maxCol) = (lines.size - 1, lines.head.size - 1)

def envelope(x: Int, y: Int): Set[(Int, Int)] =
  val above = if y > 0 then y - 1 else y
  val below = if y < maxRow then y + 1 else y
  val left = if x > 0 then x - 1 else x
  val right = if x < maxCol then x + 1 else x
  Set(
    (above, x),
    (below, x),
    (y, left),
    (y, right),
    (above, left),
    (above, right),
    (below, left),
    (below, right)
  )

def charEnvelope(x: Int, y: Int): Set[Char] =
  envelope(x, y).map((y, x) => lines(y)(x))

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

def starsInEnvelope(x: Int, y: Int): Set[(Int, Int)] =
  envelope(x, y).filter((row, col) => lines(row)(col) == '*')

import collection.mutable.{Map, Set}
val stars: Map[(Int, Int), Set[Int]] = Map()

def processLine(line: String, y: Int): Unit =
  number
    .findAllMatchIn(line)
    .foreach(m =>
      (m.start to m.end - 1).foreach(x =>
        starsInEnvelope(x, y)
          .foreach((x, y) =>
            if stars.contains((x, y)) then stars((x, y)).add(m.matched.toInt)
            else stars((x, y)) = Set(m.matched.toInt)
          )
      )
    )

lines.zipWithIndex.foreach(processLine)
println(
  stars.values
    .filter(_.size == 2)
    .map(_.toList)
    .map(l => l(0) * l(1))
    .sum
)
