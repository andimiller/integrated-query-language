package net.andimiller.iql.utils

import io.circe._, io.circe.syntax._

object CirceHelpers {

  implicit class PathCreatingCursor(cursor: ACursor) {
    def path(s: String): ACursor = {
      val attempt = cursor.downField(s)
      attempt.failed match {
        case true =>
          cursor.withFocus(_.mapObject(_.add(s, Json.obj()))).downField(s)
        case false => attempt
      }
    }
  }

  object CirceMatchers {
    object JNumber {
      def unapply(j: Json): Option[Double] =
        Option(j).flatMap(_.asNumber.map(_.toDouble))
    }
    object JInteger {
      def unapply(j: Json): Option[Int] =
        Option(j).flatMap(_.asNumber.flatMap(_.toInt))
    }
    object JBoolean {
      def unapply(j: Json): Option[Boolean] = Option(j).flatMap(_.asBoolean)
    }
    object JArray {
      def unapply(j: Json): Option[Vector[Json]] = Option(j).flatMap(_.asArray)
    }
    object JFloat {
      def unapply(j: Json): Option[Double] =
        Option(j).flatMap(_.asNumber).map(_.toDouble)
    }
    object JString {
      def unapply(j: Json): Option[String] = Option(j).flatMap(_.asString)
    }
    object JObject {
      def unapply(j: Json): Option[JsonObject] = Option(j).flatMap(_.asObject)
    }
  }

}
