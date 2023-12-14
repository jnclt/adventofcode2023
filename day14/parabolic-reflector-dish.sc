#! /usr/bin/env -S scala-cli shebang

val rows = io.Source
  .fromFile("input.txt")
  .getLines
  .map(_.map(_ match {
    case '.' => None
    case '#' => Some(false)
    case 'O' => Some(true)
  }).toList)

def fall(
    v: Option[Boolean],
    col: List[Option[Boolean]]
): List[Option[Boolean]] =
  col match
    case Some(true) :: tail if v.isEmpty => col.head :: fall(v, col.tail)
    case _                               => v :: col

val tilted =
  rows.foldRight(List.fill(100)(List[Option[Boolean]]()))((row, acc) =>
    (row zip acc).map(Function.tupled(fall))
  )

val load = tilted.transpose.reverse.zipWithIndex
  .map((row, i) => row.filter(_ == Some(true)).size * (i + 1))
  .sum

println(load)
