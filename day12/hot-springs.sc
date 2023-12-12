#! /usr/bin/env -S scala-cli shebang

case class Record(row: List[Option[Boolean]], counts: List[Int])

def toRecord(line: String): Record =
  val s"$l $r" = line: @unchecked
  val row = l.toCharArray.map(_ match
    case '?' => None
    case '#' => Some(true)
    case '.' => Some(false)).toList
  val counts = r.split(',').map(_.toInt).toList
  Record(row, counts) 

def allCombinations(row: List[Option[Boolean]]): List[List[Boolean]] = 
  row match
    case Nil => List(List())
    case x::xs => 
      val rec = allCombinations(xs)
      x match
        case Some(b) => rec.map(b::_)
        case None => rec.map(true::_) ++ rec.map(false::_)

def validCombinations(record: Record): List[List[Boolean]] =
  allCombinations(record.row).filter(r =>
    val rCounts = r.map(v => if v then '#' else '.').mkString.split('.').filter(_ != "").map(_.size).toList
    rCounts == record.counts
  )

println(io.Source.fromFile("input.txt").getLines.map(toRecord).map(validCombinations).map(_.size).sum)