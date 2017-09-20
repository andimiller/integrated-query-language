package net.andimiller.iql

import cats.implicits._, cats._, cats.data._, cats.syntax._
import io.circe.Json

object CompositionDemo extends App {


  import syntax._

  val program = List(
    ".b = .a",
    ".c = .a",
    ".d = (.a + .a)"
  ).map {
    _.unsafeIQLParse
  }.map {
    _.compile
  }.reduce {
    _ andThen _
  }.run(Compiler.State(Json.obj("a" -> Json.fromInt(1)), Json.obj()))

  println(program.unsafeRunSync().output.spaces2)


  println(iql".result = (1 + 2)".compile.run(Compiler.State.empty).unsafeRunSync().output)
}
