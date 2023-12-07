#!/usr/bin/env -S scala-cli shebang

case class Hand(cards: Array[Char], bid: Int)

val hands = io.Source
  .fromFile("input.txt")
  .getLines
  .map(line =>
    val (s"$cards $bid") = line: @unchecked
    val mapped = cards.toCharArray.map(_ match
      case 'T' => 'A'
      case 'J' => 'B'
      case 'Q' => 'C'
      case 'K' => 'D'
      case 'A' => 'E'
      case c   => c
    )
    Hand(mapped, bid.toInt)
  )

def eval(h: Hand): Long =
  val secondary = Integer.parseInt(h.cards.mkString, 16).toLong
  val labelCounts =
    h.cards.distinct.map(label => h.cards.count(_ == label)).sorted.reverse
  val primary = labelCounts.size match
    case 1                          => 1000_000 // Five of a kind
    case 2 if labelCounts.head == 4 => 100_000 // Four of a kind
    case 2 if labelCounts.head == 3 => 10_000 // Full house
    case 3 if labelCounts.head == 3 => 1000 // Three of a kind
    case 3 if labelCounts.head == 2 => 100 // Two pair
    case 4                          => 10 // One pair
    case 5                          => 1 // High card
  primary * secondary

val totalWinnings = hands.toVector
  .sortBy(eval)
  .zipWithIndex
  .map((hand, rank) => (rank + 1) * hand.bid)
  .sum
println(totalWinnings)
