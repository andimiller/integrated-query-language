package net.andimiller.iql.examples

import io.circe._
import io.circe.parser._
import net.andimiller.iql.{Compiler, Parser}

object CirceTest extends App {
  /*val program =
     """.a.value = "a"
       |.b = "b"
       |.a.b.c = "nested"
       |.a.original = .a""".stripMargin
    val inputjson =
      """{
        | "a": "originala"
        |}""".stripMargin
   */
  var exit = false
  while (!exit) {
    val program = scala.io.StdIn.readLine("iql>")
    val json    = scala.io.StdIn.readLine("json>")

    if ((program == null) || (json == null)) {
      exit = true
    }

    if (!exit) {
      val parsedprogram = Parser.program.parse(program).get.value
      val results =
        Compiler
          .programCompiler(parsedprogram)
          .run(Compiler.State(parse(json).right.toOption.getOrElse(Json.obj()), Json.obj()))
          .unsafeRunSync()
      println(results.output)
    }
  }
}
