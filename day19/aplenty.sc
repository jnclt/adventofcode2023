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
    case _ => false

val accepted = parts.filter(passed(workflows("in"))).toList
println(accepted.map(p => p("x") + p("m") + p("a") + p("s")).sum)
