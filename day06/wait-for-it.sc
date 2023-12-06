#!/usr/bin/env -S scala-cli shebang

// with limit T (-b), goal D (c), push time (x) and result distance (y):
//     y = (T - x) * x = Tx - x^2
//     y > D
//     x^2 - Tx + D = 0
//     x = (-b +- sqrt(b^2 - 4ac)) / 2a
//     x = (T +- sqrt(T^2 - 4D)) / 2)

// Time:        62     64     91     90
// Distance:   553   1010   1473   1074

def estSqrt(x: BigInt): BigInt =
  def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1

  val one = BigInt(1)
  var n = one
  var n1 = next(n, x)
  while ((n1 - n).abs > one)
    n = n1
    n1 = next(n, x)
  while (n1 * n1 > x)
    n1 -= one
  n1 + one

def root(r: Race): BigInt =
  (r.T - estSqrt((r.T * r.T) - (4 * r.D))) / 2

case class Race(T: BigInt, D: BigInt)
val races = Seq(Race(62, 553), Race(64, 1010), Race(91, 1473), Race(90, 1074))

println(races.map(r => r.T + 1 - (2 * (root(r) + 1))).product)

val r = Race(62649190, BigInt("553101014371074"))
println(r.T + 1 - (2 * (root(r) + 1)))
