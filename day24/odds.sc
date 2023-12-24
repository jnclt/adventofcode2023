#! /usr/bin/env -S scala-cli shebang

val (min, max) = (2e14, 4e14)
val dirs = io.Source
  .fromFile("input.txt")
  .getLines
  .map(l =>
    val s"$x, $y, $z @ $vx, $vy, $vz" = l: @unchecked
    (
      (x.trim.toLong, y.trim.toLong),
      (vx.trim.toDouble, vy.trim.toDouble)
    )
  )
  .toList

def time(
    a: ((Long, Long), (Double, Double)),
    b: ((Long, Long), (Double, Double))
): Double =
  val ((xa, ya), (vxa, vya)) = a
  val ((xb, yb), (vxb, vyb)) = b
  if vxa == 0 then -1
  else
    val denom = vya * vxb - vyb * vxa
    if denom == 0 then -1
    else (vxb * (yb - ya) + vyb * (xa - xb)) / denom

def cross(
    a: ((Long, Long), (Double, Double)),
    b: ((Long, Long), (Double, Double))
): Boolean =
  val t1 = time(a, b)
  val ((xa, ya), (vxa, vya)) = a
  val ((xb, yb), (vxb, vyb)) = b
  val (x, y) = (xa + vxa * t1, ya + vya * t1)
  val t2 = (x - xb) / vxb
  t1 >= 0 && t2 >= 0 && x >= min && x <= max && y >= min && y <= max

val crossed = dirs.combinations(2).filter(p => cross(p(0), p(1)))
println(crossed.size)
