#! /usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines()
val directions = lines.next.map(_ match { case 'L' => 0 case 'R' => 1 })
lines.next
val map = lines.map(line =>
  val s"${n} = (${l}, ${r})" = line: @unchecked 
  n -> (l, r)
  ).toMap

val stepCount = LazyList.continually(directions).flatten
  .scanLeft(("AAA", 0)){case ((place, count), dir) => (map(place)(dir).toString, count + 1) }
  .dropWhile(_._1 != "ZZZ")
  .head._2
println(stepCount)
