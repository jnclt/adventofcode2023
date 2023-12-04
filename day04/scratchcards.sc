#!/usr/bin/env -S scala-cli shebang

import Math.{max, pow}

val lines = io.Source.fromFile("input.txt").getLines
println(
  lines
    .map(_.drop(9))
    .map { card =>
      val (s"$winning | $draw") = card: @unchecked
      val w = winning.trim.split("""\s+""").map(_.toInt).toSet
      val d = draw.trim.split("""\s+""").map(_.toInt).toSet
      (w & d).toList match {
        case Nil => 0
        case l   => pow(2, l.size - 1).toInt
      }
    }
    .sum
)
