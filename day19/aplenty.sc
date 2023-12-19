#! /usr/bin/env -S scala-cli shebang

type Part = Map[String, Int]

val lines = io.Source.fromFile("input.txt").getLines
val workflows = lines
  .takeWhile(!_.isEmpty)
  .map(l =>
    val s"$id{$body}" = l: @unchecked
    (id -> body.split(',').toList)
  )
  .toMap + ("R" -> List("R")) + ("A" -> List("A"))
val parts = lines.map(l =>
  val s"{x=$x,m=$m,a=$a,s=$s}" = l: @unchecked
  Map("x" -> x.toInt, "m" -> m.toInt, "a" -> a.toInt, "s" -> s.toInt)
)

def passed(wf: List[String])(part: Part): Boolean =
  wf match
    case "A" :: nil => true
    case rule :: rest =>
      val next = rule match
        case s"${att}>${value}:${name}" if part(att) > value.toInt =>
          workflows(name)
        case s"${att}<${value}:${name}" if part(att) < value.toInt =>
          workflows(name)
        case name if """([a-z]+)""".r matches name => workflows(name)
        case _                                     => rest
      passed(next)(part)
    case _ => false // "R" :: nil

val accepted = parts.filter(passed(workflows("in"))).toList
println(accepted.map(p => p("x") + p("m") + p("a") + p("s")).sum)

type PartRange = Map[String, Range]
case class Node(partRange: PartRange, succ: Set[Node])

def intersect(l: Range, r: Range): Range =
  (l.start max r.start) to (l.end min r.end)
def leaves(wf: List[String], partRange: PartRange): List[PartRange] =
  if !partRange.values.forall(_.nonEmpty) then List()
  else
    wf match
      case "A" :: nil => List(partRange)
      case "R" :: nil => List()
      case rule :: rest =>
        rule match
          case s"${att}>${value}:${name}" =>
            val lRange: PartRange = partRange.updated(
              att,
              intersect(partRange(att), value.toInt + 1 to 4000)
            )
            val rRange: PartRange = partRange.updated(
              att,
              intersect(partRange(att), 1 to value.toInt)
            )
            leaves(workflows(name), lRange) ++ leaves(rest, rRange)
          case s"${att}<${value}:${name}" =>
            val lRange: PartRange = partRange.updated(
              att,
              intersect(partRange(att), 1 to value.toInt - 1)
            )
            val rRange: PartRange = partRange.updated(
              att,
              intersect(partRange(att), value.toInt to 4000)
            )
            leaves(workflows(name), lRange) ++ leaves(rest, rRange)
          case name => leaves(workflows(name), partRange)

val acceptedRanges =
  leaves(workflows("in"), "xmas".split("").zip(List.fill(4)(1 to 4000)).toMap)
val total = acceptedRanges
  .map(_.values.map(_.size.toLong).product)
  .sum
println(total)
