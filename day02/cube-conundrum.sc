#!/usr/bin/env -S scala-cli shebang

case class Round(red: Int, green: Int, blue: Int)
case class Game(no: Int, rounds: List[Round])

def parse(line: String): Game =
  def parseRound(s: String): Round =
    val counts = s
      .split(", ")
      .map(item => { val (l, r) = item.span(_ != ' '); (l.toInt, r.drop(1)) })
    val countMap = Map(counts.map(c => c._2 -> c._1): _*).withDefaultValue(0)
    Round(countMap("red"), countMap("green"), countMap("blue"))

  val (l, r) = line.span(_ != ':')
  Game(
    l.drop(5).toInt,
    r.drop(2).split("; ").toList.map(parseRound(_))
  )

def possible(game: Game): Boolean =
  game.rounds.forall(r => r.red <= 12 && r.green <= 13 && r.blue <= 14)

def minimumPower(game: Game): Int =
  val min = game.rounds.foldLeft(Round(0, 0, 0))((curr, next) =>
    Round(
      Vector(curr.red, next.red).max,
      Vector(curr.green, next.green).max,
      Vector(curr.blue, next.blue).max
    )
  )
  min.red * min.green * min.blue

val games = io.Source.fromFile("input.txt").getLines.map(parse).toList
println(games.filter(possible).map(_.no).sum)
println(games.map(minimumPower).sum)
