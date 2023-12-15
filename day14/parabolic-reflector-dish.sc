#! /usr/bin/env -S scala-cli shebang

val rows = io.Source
  .fromFile("input.txt")
  .getLines
  .map(_.map(_ match {
    case '.' => None
    case '#' => Some(false)
    case 'O' => Some(true)
  }).toList)
  .toList

def fall(
    v: Option[Boolean],
    col: List[Option[Boolean]]
): List[Option[Boolean]] =
  col match
    case Some(true) :: tail if v.isEmpty => col.head :: fall(v, col.tail)
    case _                               => v :: col

type Platform = List[List[Option[Boolean]]]

def tilt(rows: Platform) =
  rows.foldRight(List.fill(100)(List[Option[Boolean]]()))((row, acc) =>
    (row zip acc).map(Function.tupled(fall))
  )

def load(rows: Platform) =
  rows.reverse.zipWithIndex
    .map((row, i) => row.filter(_ == Some(true)).size * (i + 1))
    .sum

println(load(tilt(rows).transpose))

// part 2

def cycle(rows: Platform): Platform =
  tilt(
    tilt(
      tilt(tilt(rows /*north*/ ) /*west*/ ).reverse /*south*/
    ).reverse /*east*/
  ).reverse.map(_.reverse)

def loop(results: List[Platform]): List[Platform] =
  val c = cycle(results.head)
  if results.contains(c) then c :: results
  else loop(c :: results)

val results = loop(List(rows))
val loopLen = results.tail.indexOf(results.head) + 1
val loopStart = results.size - loopLen - 1
val idx = (1000_000_000 - loopStart) % loopLen
println(load(results.reverse(loopStart + idx)))
