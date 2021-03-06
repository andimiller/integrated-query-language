package net.andimiller.iql.examples

import io.circe._
import io.circe.parser._
import net.andimiller.iql.{Compiler, Parser}

object CirceTest2 extends App {
  var exit = false
  while (!exit) {
    val program = scala.io.StdIn.readLine("iql>")
    val json    = scala.io.StdIn.readLine("json>")

    if ((program == null) || (json == null)) {
      exit = true
    }

    if (!exit) {
      val parsedprogram = Parser.validationProgram.parse(program).get.value
      val results =
        Compiler
          .vprogramCompiler(parsedprogram)
          .run(parse(json).right.toOption.getOrElse(Json.obj()))
          .unsafeRunSync()
      println(results)
    }
  }
}
