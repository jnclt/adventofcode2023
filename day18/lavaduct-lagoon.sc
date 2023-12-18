#! /usr/bin/env -S scala-cli shebang
import scala.annotation.tailrec

val steps = io.Source
  .fromFile("input.txt")
  .getLines
  .map(l =>
    val s"$dir $n $c" = l: @unchecked
    (dir(0), n.toInt, c)
  )
  .toList

val loop = steps
  .foldLeft((0, 0), Set[(Int, Int)]()) { case ((at, acc), s) =>
    val dig = s._1 match
      case 'R' => for x <- 1 to s._2 yield (at._1, at._2 + x)
      case 'L' => for x <- 1 to s._2 yield (at._1, at._2 - x)
      case 'D' => for y <- 1 to s._2 yield (at._1 + y, at._2)
      case 'U' => for y <- 1 to s._2 yield (at._1 - y, at._2)
    (dig.last, acc ++ dig.toSet)
  }
  ._2

@tailrec
def fill(from: Set[(Int, Int)], acc: Set[(Int, Int)]): Set[(Int, Int)] =
  val next = from.flatMap(at =>
    Set(
      (at._1 + 1, at._2),
      (at._1 - 1, at._2),
      (at._1, at._2 + 1),
      (at._1, at._2 - 1)
    )
  ) -- acc
  if next.isEmpty then acc
  else fill(next, acc ++ from)

val lagoon = fill(Set((1, 1)), loop)
println(lagoon.size + 1)
