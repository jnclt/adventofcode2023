#! /usr/bin/env -S scala-cli shebang

val maze = io.Source.fromFile("input.txt").getLines.toVector.map(_.toCharArray)

def next(from: (Int, Int), prev: (Int, Int)): (Int, Int) =
    maze(from._1)(from._2) match
        case '-' if prev._2 > from._2 => (from._1, from._2 - 1)  // from right to left
        case '-' if prev._2 < from._2 => (from._1, from._2 + 1)  // from left to right
        case '|' if prev._1 > from._1 => (from._1 - 1, from._2)  // from below up
        case '|' if prev._1 < from._1 => (from._1 + 1, from._2)  // from above down
        case 'L' if prev._1 < from._1 => (from._1, from._2 + 1)  // from above to right
        case 'L' if prev._2 > from._2 => (from._1 - 1, from._2)  // from right up
        case 'F' if prev._1 > from._1 => (from._1, from._2 + 1)  // from below to right
        case 'F' if prev._2 > from._2 => (from._1 + 1, from._2)  // from right down
        case 'J' if prev._1 < from._1 => (from._1, from._2 - 1)  // from above to left
        case 'J' if prev._2 < from._2 => (from._1 - 1, from._2)  // from left up
        case '7' if prev._1 > from._1 => (from._1, from._2 - 1)  // from below to left
        case '7' if prev._2 < from._2 => (from._1 + 1, from._2)  // from left down

def cycle(from: (Int, Int), acc: Vector[(Int, Int)]): Vector[(Int, Int)] =
    if from == acc.head then acc :+ from
    else cycle(next(from, acc.last), acc :+ from)

val (start, second) = ((62, 61), (63, 61))  // manual lookup
println(cycle(second, Vector(start)).size / 2)