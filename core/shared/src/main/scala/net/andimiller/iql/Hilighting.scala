package net.andimiller.iql

import fastparse.all._

import scala.collection.mutable

object Hilighting extends App {

  def analyse(s: String) = {

    val callCount = mutable.Buffer.empty[(String, (Int, Int))]
    val instrumentFunction = (parser: Parser[_], index: Int, continuation: () => Parsed[_]) => {
      val start = index
      val result = continuation()
      val end = result.index
      callCount.append((parser.toString, (start, end)))
    }

    val parsed = Parser.assignment.parse(s, instrument = instrumentFunction)
    val results = callCount.toIterator.filter { case (p, (start, end)) => end > start }.toList

    val classifications = (0 to s.size)
     .map{ idx =>
       idx -> results.filter{case (_, (start, end)) => (start <= idx) && (end >= idx)}
     }.toMap.mapValues(_.map(_._1)).toMap
    println(classifications)

  }

  analyse(".result = 1 + 2")

}
