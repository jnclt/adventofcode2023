#!/usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines.toVector

case class Hand(cards: Array[Char], bid: Int)

def simpleMap(c: Char): Char = c match
  case 'T' => 'A'
  case 'J' => 'B'
  case 'Q' => 'C'
  case 'K' => 'D'
  case 'A' => 'E'
  case c   => c

def jokerMap(c: Char): Char = c match
  case 'T' => 'A'
  case 'Q' => 'B'
  case 'K' => 'C'
  case 'A' => 'D'
  case 'J' => '1'
  case c   => c

def getHands(map: Char => Char): Vector[Hand] = lines
  .map(line =>
    val (s"$cards $bid") = line: @unchecked
    Hand(cards.toCharArray.map(map), bid.toInt)
  )

def totalWinnings(map: Char => Char): Long =
  getHands(map)
    .sortBy(eval)
    .zipWithIndex
    .map((hand, rank) => (rank + 1) * hand.bid)
    .sum

def eval(h: Hand): (Int, String) =
  val cards = h.cards.filter(_ != '1')
  val jokerCount = h.cards.size - cards.size
  val labelCounts = if jokerCount == 5 then Array(0) else
    cards.distinct
      .map(label => cards.count(_ == label))
      .sorted
      .reverse
  labelCounts(0) = labelCounts(0) + jokerCount
  val `type` = labelCounts.size match
    case 1                          => 1000_000 // Five of a kind
    case 2 if labelCounts.head == 4 => 100_000 // Four of a kind
    case 2 if labelCounts.head == 3 => 10_000 // Full house
    case 3 if labelCounts.head == 3 => 1000 // Three of a kind
    case 3 if labelCounts.head == 2 => 100 // Two pair
    case 4                          => 10 // One pair
    case 5                          => 1 // High card
  (`type`, h.cards.mkString)

println(totalWinnings(simpleMap))
println(totalWinnings(jokerMap))
