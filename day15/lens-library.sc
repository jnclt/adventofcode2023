#! /usr/bin/env -S scala-cli shebang

val steps = io.Source.fromFile("input.txt").getLines.next.split(',')
def hash(s: String): Int =
  s.foldLeft(0)((acc, c) => ((acc + c.toInt) * 17) % 256)
println(steps.map(hash).sum)

// part 2

val hashmap: Array[Array[(String, Int)]] = Array.fill(256)(Array())

enum Op:
  case In(len: Int)
  case Out

case class Step(label: String, op: Op)

def exec(step: Step): Unit =
  val boxIdx = hash(step.label)
  val box = hashmap(boxIdx)
  hashmap(boxIdx) = step.op match
    case Op.Out => box.filter(_._1 != step.label)
    case Op.In(len) =>
      box.indexWhere(_._1 == step.label) match
        case -1 => box.appended((step.label, len))
        case i  => box.take(i) :+ (step.label, len) :++ box.drop(i + 1)

def eval(): Int = hashmap.zipWithIndex
  .map((box, i) =>
    (i + 1) * box.zipWithIndex.map { case ((_, len), j) => (j + 1) * len }.sum
  )
  .sum

steps
  .map(_ match
    case s"${label}-"       => Step(label, Op.Out)
    case s"${label}=${len}" => Step(label, Op.In(len.toInt))
  )
  .map(exec)

println(eval())
