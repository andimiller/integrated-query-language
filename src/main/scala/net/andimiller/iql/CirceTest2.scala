package net.andimiller.iql

import io.circe._, io.circe.parser._

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
          .run(Compiler.State(parse(json).getOrElse(Json.obj()), Json.obj()))
          .unsafeRunSync()
      println(results._1.output)
    }
  }
}
