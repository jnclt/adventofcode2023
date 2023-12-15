#! /usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines.toList
val grids = lines
  .foldLeft(List(List[String]()))((grids, line) =>
    if line.isEmpty() then List() :: grids
    else (line :: grids.head) :: grids.tail
  )
  .map(_.reverse)
  .reverse

def split(grid: List[String], skip: Option[Int]): Option[Int] =
  LazyList
    .range(1, grid.size)
    .filter(i =>
      skip match {
        case None    => true
        case Some(s) => i != s
      }
    )
    .map(grid.splitAt(_))
    .find((l, r) =>
      val len = l.size min r.size
      l.reverse.take(len) == r.take(len)
    )
    .map(_._1.size)

def score(grid: List[String]): Int =
  split(grid, None) match
    case Some(n) => n * 100
    case None    => split(grid.transpose.map(_.mkString), None).get

val scores = grids.map(score)
println(scores.sum)

// part 2

def smudge(grid: List[String]): LazyList[List[String]] =
  val allCoords = LazyList
    .range(0, grid.size)
    .flatMap(y => LazyList.range(0, grid.head.size).map((y, _)))
  allCoords.map((y, x) =>
    grid.updated(y, grid(y).updated(x, if grid(y)(x) == '.' then '#' else '.'))
  )

def rescore(grid: List[String], origScore: Int): Option[Int] =
  origScore % 100 match
    case 0 =>
      split(grid, Some(origScore / 100)) match
        case Some(n) => Some(n * 100)
        case None    => split(grid.transpose.map(_.mkString), None)
    case _ =>
      split(grid, None) match
        case Some(n) => Some(n * 100)
        case None    => split(grid.transpose.map(_.mkString), Some(origScore))

def scoreSmudged(grid: List[String], origScore: Int): Int =
  val hit = smudge(grid).find(sg => rescore(sg, origScore).isDefined).get
  rescore(hit, origScore).get

val scores2 = grids.zip(scores).map(scoreSmudged)
println(scores2.sum)
