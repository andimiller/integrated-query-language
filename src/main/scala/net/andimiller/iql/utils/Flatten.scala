package net.andimiller.iql.utils

import cats.{Applicative, Eval, Traverse}
import io.circe.{ACursor, Json, JsonNumber, JsonObject}

object Flatten {

  import CirceHelpers._
  import CirceMatchers._

  def flatten(j: Json): Json = iflatten(j).top.get

  def iflatten(j: Json, path: List[String] = List(), output: ACursor = Json.obj().hcursor.asInstanceOf[ACursor]): ACursor = {
    val here = "."+path.mkString(".")
    j match {
      case JObject(o) =>
        o.keys.foldLeft(output){case (cursor, key) => iflatten(o.apply(key).get, path :+ key, cursor)}
      case JArray(a) =>
        a.zipWithIndex.foldLeft(output) { case (cursor, (json, index)) => iflatten(json, path :+ s"[$index]", cursor) }
      case _ =>
        output.path(here).withFocus(_ => j).up
    }
  }
}
