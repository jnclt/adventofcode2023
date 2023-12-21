#! /usr/bin/env -S scala-cli shebang
import scala.annotation.tailrec

val lines = io.Source.fromFile("input.txt").getLines.toList
val (width, height) = (lines.head.size, lines.size)

def xy(i: Int): (Int, Int) = (i % width, i / width)

val line = lines.mkString
val start = Set(xy(line.indexOf('S')))
val rocks = line.zipWithIndex.filter(_._1 == '#').map(p => xy(p._2)).toSet

def neighbors(of: Set[(Int, Int)]): Set[(Int, Int)] =
  of.flatMap((x, y) => Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)))
    .filter((x, y) => 0 <= x && x < width && 0 <= y && y < height) -- rocks

@tailrec
def step(from: Set[(Int, Int)], counter: Int): Set[(Int, Int)] =
  if counter == 0 then from else step(neighbors(from), counter - 1)

println(step(start, 64).size)
