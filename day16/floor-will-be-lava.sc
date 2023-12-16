#! /usr/bin/env -S scala-cli shebang

val grid = io.Source.fromFile("input.txt").getLines.toVector

val (maxRow, maxCol) = (grid.size - 1, grid.head.size - 1)

case class Point(y: Int, x: Int):
  def +(that: Point) = (y + that.y, x + that.x) match
    case (row, col) if row < 0 || col < 0           => None
    case (row, col) if row > maxRow || col > maxCol => None
    case (row, col)                                 => Some(Point(row, col))

def turnBackslash(dir: Point) = Point(dir.x * -1, dir.y * -1)
def turnSlash(dir: Point) = Point(dir.x, dir.y)

def tile(at: Point) = grid(at.y)(at.x)

def beam(): Set[Point] =
  var moves = List((Point(0, -1), Point(0, 1)))
  var visited = Set[(Point, Point)]()

  while (moves.nonEmpty)
    val (from, dir) = moves.head
    moves = moves.tail
    if (from + dir).isDefined then
      val at = (from + dir).get
      if !visited.contains((at, dir)) then
        visited += ((at, dir))
        tile(at) match
          case '/' =>
            moves ::= (at, turnBackslash(dir))
          case '\\' => moves ::= (at, turnSlash(dir))
          case '|' if dir.y == 0 =>
            moves ::= (at, Point(1, 0))
            moves ::= (at, Point(-1, 0))
          case '-' if dir.x == 0 =>
            moves ::= (at, Point(0, 1))
            moves ::= (at, Point(0, -1))
          case _ => moves ::= (at, dir)
  visited.map(_._1)

val light = beam()
println(light.size)
