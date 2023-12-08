#! /usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines()
val directions = lines.next.map(_ match { case 'L' => 0 case 'R' => 1 }).toList
lines.next
val map = lines.map(line =>
  val s"${n} = (${l}, ${r})" = line: @unchecked 
  n -> (l, r)
  ).toMap

def stepCount(end: String => Boolean)(start: String): Int = 
    LazyList.continually(directions).flatten
  .scanLeft((start, 0)){case ((place, count), dir) => (map(place)(dir).toString, count + 1) }
  .dropWhile((place, _) => !end(place))
  .head._2

println(stepCount(_ == "ZZZ")("AAA"))

val counts = map.keys.filter(_.endsWith("A")).map(stepCount(_.endsWith("Z"))).map(BigInt(_))

def leastCommonMultiple(values: Iterable[BigInt]): BigInt =
    val multiples = LazyList.iterate(values.head)(_ + values.head)
    multiples.find(m => values.forall(m % _ == 0)).get

println(
leastCommonMultiple(Set(
  leastCommonMultiple(counts.take(counts.size / 2)),
  leastCommonMultiple(counts.drop(counts.size / 2))
)))