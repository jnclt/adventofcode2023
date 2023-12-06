#!/usr/bin/env -S scala-cli shebang

import scala.collection.immutable.NumericRange

val lines = io.Source.fromFile("input.txt").getLines
val seeds = lines.next.drop(7).split(" ").map(BigInt(_)).toVector
lines.next // skip empty line after seeds

type Range = NumericRange.Inclusive[BigInt]
case class RangeOffset(r: Range, o: BigInt)
type RangeMap = Vector[RangeOffset]

def parseMap(): RangeMap =
  lines
    .drop(1)
    .takeWhile(!_.isEmpty)
    .map(_.split(" ").map(BigInt(_)))
    .toVector
    .map(record =>
      RangeOffset(
        (record(1) to record(1) + record(2) - 1),
        (record(0) - record(1))
      )
    )
    .toVector

def mapTo(value: BigInt, rangeMap: RangeMap): BigInt =
  rangeMap.find(_.r.contains(value)) match
    case Some(record) => value + record.o
    case None         => value

val maps = (1 to 7).map(_ => parseMap())
val location = seeds.map { seed =>
  maps.foldLeft(seed)(mapTo)
}.min

println(location)

val ranges: Vector[Range] =
  (0 until seeds.size by 2)
    .map(i => (seeds(i) to (seeds(i) + seeds(i + 1) - 1)))
    .sorted
    .toVector

def intersect(a: Range, b: Range): Option[Range] =
  if a.start > b.end || b.start > a.end then None
  else Some((a.start max b.start) to (a.end min b.end))

def applyRecordToRanges(
    ranges: Vector[Range],
    record: RangeOffset
): (Vector[Range], Vector[Range]) =
  val (mapped, unmapped) = ranges.map(applyRecord(_, record)).unzip
  (mapped.flatten, unmapped.flatten)

def applyRecord(
    range: Range,
    record: RangeOffset
): (Option[Range], Vector[Range]) =
  intersect(range, record.r) match
    case None => (None, Vector(range))
    case Some(r) =>
      val left =
        if range.start < record.r.start then
          Vector((range.start to record.r.start - 1))
        else Vector()
      val right =
        if range.end > record.r.end then Vector((record.r.end + 1 to range.end))
        else Vector()
      (Some(r.start + record.o to r.end + record.o), left ++ right)

def rangeMapTo(ranges: Vector[Range], rangeMap: RangeMap): Vector[Range] =
  val (mapped, unmapped) =
    rangeMap.foldLeft((Vector(): Vector[Range], ranges)) {
      case ((mapped, unmapped), record) =>
        val (newMapped, newUnmapped) = applyRecordToRanges(unmapped, record)
        (mapped ++ newMapped, newUnmapped)
    }
  mapped ++ unmapped

val minimumLocation = maps
  .foldLeft(ranges)(rangeMapTo)
  .sorted
  .head
  .start
println(minimumLocation)
