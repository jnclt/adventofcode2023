#! /usr/bin/env -S scala-cli shebang

import Math.abs

val lines = io.Source.fromFile("input.txt").getLines().toVector
val colCount = lines.head.size
val rowCount = lines.size

def expand(indices: Iterable[Int], idxToString: Int => Iterable[Char], factor: Int): Map[Int, Int] =
  indices.foldLeft((0, Vector[(Int, Int)]())) { case ((acc, ys), idx) =>
  idxToString(idx).count(_ == '#') match
    case 0 => (acc + factor, ys)
    case _ => (acc + 1, ys :+ (idx -> acc))
}._2.toMap

def distances(factor: Int): Iterator[Int] =
  val ys = expand(0 to rowCount - 1, lines(_), factor)
  val xs = expand(0 to colCount - 1, idx => lines.map(_(idx)), factor)
  val positions = lines.zipWithIndex.flatMap((line, y) =>
    line.zipWithIndex.filter(_._1 == '#').map((_, x) => (ys(y), xs(x))))  

  positions.combinations(2).map{ case Vector(a, b) => abs(b._1 - a._1) + abs(b._2 - a._2) }

println(distances(2).sum)
println(distances(1_000_000).map(_.toLong).sum)
