#!/usr/bin/env -S scala-cli shebang

import scala.collection.immutable.NumericRange

val lines = io.Source.fromFile("input.txt").getLines
val seeds = lines.next.drop(7).split(" ").map(BigInt(_)).toSet
lines.next // skip empty line after seeds

type RangeMap = Vector[(NumericRange.Inclusive[BigInt], BigInt)]

def parseMap(): RangeMap =
  lines
    .drop(1)
    .takeWhile(!_.isEmpty)
    .map(_.split(" ").map(BigInt(_)))
    .toVector
    .map(record => ((record(1) to record(1) + record(2) - 1) -> record(0)))
    .toVector

def mapTo(value: BigInt, rangeMap: RangeMap): BigInt =
  rangeMap.find(_._1.contains(value)) match
    case Some(record) => record._2 + (value - record._1.start)
    case None         => value

val maps = (1 to 7).map(_ => parseMap())
val location = seeds.map { seed =>
  maps.foldLeft(seed)(mapTo)
}.min

println(location)
