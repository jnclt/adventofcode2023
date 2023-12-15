#! /usr/bin/env -S scala-cli shebang

val steps = io.Source.fromFile("input.txt").getLines.next.split(',')
def hash(s: String): Int =
  s.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)
println(steps.map(hash).sum)
