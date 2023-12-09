#! /usr/bin/env -S scala-cli shebang

val histories = io.Source.fromFile("input.txt").getLines.map(_.split(" ").toVector.map(_.toInt)).toList

def predict(history: Vector[Int]): Int =
    if history.forall(_ == 0) then 0
    else history.last + predict(history.sliding(2).map(w => w(1) - w(0)).toVector)

println(histories.map(predict).sum)
println(histories.map(_.reverse).map(predict).sum)
