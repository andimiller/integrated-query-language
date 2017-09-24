package net.andimiller.iql

import io.circe.{ACursor, Json}
import io.circe.parser.parse
import org.scalatest.{FlatSpec, MustMatchers}
import cats._
import cats.syntax._
import cats.implicits._

class FlattenSpec extends FlatSpec with MustMatchers {
  import utils.Flatten

  "Flatten" should "work on a simple object" in {
    Flatten.flatten(Json.obj("a" -> Json.fromInt(2))) must equal(Json.obj(".a" -> Json.fromInt(2)))
  }
  "Flatten" should "work on some serious nested stuff" in {
    import utils.CirceHelpers.PathCreatingCursor
    val input  = List.fill(100)("a").foldLeft(Json.obj().hcursor.asInstanceOf[ACursor]) { _ path _ }.withFocus(_ => Json.fromInt(1)).top.get
    val result = Flatten.flatten(input)
    result must equal(Json.obj("." + List.fill(100)("a").mkString(".") -> Json.fromInt(1)))
  }
  "Flatten" should "be available as syntax" in {
    import syntax.JsonSyntax
    Json.obj("a" -> Json.fromInt(2)).flatten must equal(Json.obj(".a" -> Json.fromInt(2)))
  }

  "Flatten" should "work on some json" in {
    val j = parse("""{
      |  "a": 1,
      |  "b": 2,
      |  "c": 3,
      |  "d": 4
      |}""".stripMargin).right.get
    println(j)
    Flatten.flatten(j)

  }
}
