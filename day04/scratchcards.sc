#!/usr/bin/env -S scala-cli shebang

import Math.{min, pow}

val lines = io.Source.fromFile("input.txt").getLines.toVector
val matchCounts = lines
  .map(_.drop(9))
  .map { card =>
    val (s"$winning | $draw") = card: @unchecked
    val w = winning.trim.split("""\s+""").map(_.toInt).toSet
    val d = draw.trim.split("""\s+""").map(_.toInt).toSet
    (w & d).size
  }
  .toVector

println(matchCounts.filter(_ > 0).map(s => pow(2, s - 1).toInt).sum)

println(
  matchCounts.zipWithIndex
    .foldLeft(Vector.fill(lines.size)(1)) {
      case (cardCounts, (matchCount, index)) =>
        cardCounts.take(index + 1) ++
          (index + 1 to min(index + matchCount, lines.size)).map(i =>
            cardCounts(i) + cardCounts(index)
          ) ++
          cardCounts.drop(index + 1 + matchCount)
    }
    .sum
)
