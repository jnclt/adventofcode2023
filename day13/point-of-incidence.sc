#! /usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines.toList
val grids = lines.foldLeft(List(List[String]()))((grids, line) =>
  if line.isEmpty() then List()::grids
  else (line::grids.head) :: grids.tail
  ).map(_.reverse).reverse

def split(grid: List[String]): Option[Int] =
  LazyList.range(1, grid.size).map(grid.splitAt(_)).find((l, r) => 
    val len = l.size min r.size
    l.reverse.take(len) == r.take(len)
    ).map(_._1.size)

def score(grid: List[String]): Int = 
  split(grid) match
    case Some(n) => n * 100
    case None => split(grid.transpose.map(_.mkString)).get

println(grids.map(score).sum)

