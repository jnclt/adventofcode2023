#! /usr/bin/env -S scala-cli shebang

val grid = io.Source.fromFile("input.txt").getLines.toVector
val (maxRow, maxCol) = (grid.size - 1, grid.head.size - 1)
val start = (0, grid(0).indexOf("."))
val end = (maxRow, grid(maxRow).indexOf("."))

def successors(of: List[(Int, Int)]): Set[(Int, Int)] =
  val (y, x) :: tail = of: @unchecked
  Set(
    if x > 0 && Set('.', '<').contains(grid(y)(x - 1)) then Some((y, x - 1))
    else None,
    if x < maxCol && Set('.', '>').contains(grid(y)(x + 1)) then
      Some((y, x + 1))
    else None,
    if y > 0 && Set('.', '^').contains(grid(y - 1)(x)) then Some((y - 1, x))
    else None,
    if y < maxRow && Set('.', 'v').contains(grid(y + 1)(x)) then
      Some((y + 1, x))
    else None
  ).flatten -- tail

def branch(
    leaf: (Int, Int),
    dag: Map[(Int, Int), (Int, Int)]
): List[(Int, Int)] =
  leaf :: (dag.get(leaf) match {
    case None       => Nil
    case Some(pred) => branch(pred, dag)
  })

def dag(
    edge: Set[(Int, Int)],
    dist: Int,
    acc: Map[(Int, Int), (Int, Int)]
): Map[(Int, Int), (Int, Int)] =
  val next =
    edge.flatMap(l => successors(branch(l, acc)).map((_ -> l))).toMap
  if next.isEmpty then acc
  else dag(next.keySet, dist + 1, acc.concat(next))

println(branch(end, dag(Set(start), 0, Map())).size - 1)
