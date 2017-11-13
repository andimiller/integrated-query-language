package net.andimiller.iql

import fastparse.all._

import scala.collection.mutable
import cats._, cats.implicits._, cats.syntax._

object Hilighting {

  val parserToType = List(
    "equals"          -> "topLevelOperator",
    "is"              -> "topLevelOperator",
    "outputReference" -> "reference",
    "reference"       -> "reference",
    "number"          -> "literal",
    "string"          -> "literal",
    "boolean"         -> "literal"
  )
  val parserToTypeKeys: Set[String] = parserToType.map(_._1).toSet

  val typePriority = Map(
    "topLevelOperator" -> 1,
    "reference" -> 2,
    "literal" -> 3
  )

  val defaultColours = Map(
    "topLevelOperator" -> fansi.Color.Cyan,
    "reference"        -> fansi.Color.Blue,
    "literal"          -> fansi.Color.Yellow
  )

  def analyse(s: String): fansi.Str = {

    val callCount = mutable.Buffer.empty[(String, (Int, Int))]
    val instrumentFunction = (parser: Parser[_], index: Int, continuation: () => Parsed[_]) => {
      val start = index
      val result = continuation()
      val end = result.index
      callCount.append((parser.toString, (start, end)))
    }

    val parsed = Parser.assignment.parse(s, instrument = instrumentFunction)
    val results = callCount.toIterator.filter { case (p, (start, end)) => end > start }.toList

    val syntax = results
      .filter{case (k, _) => parserToTypeKeys.contains(k)}
      .map{case (k, v) => (parserToType.find(_._1 == k).map(_._2).get, v)}


    println(syntax)

    syntax.foldLeft(fansi.Str(s)) { case (fs, (t, (s, e))) =>
      defaultColours.get(t) match {
        case Some(c) => fs.overlay(c, s, e)
        case None => fs
      }
    }
  }
}
