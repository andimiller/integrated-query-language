package net.andimiller.iql
import io.circe._, io.circe.parser._

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
    val json = scala.io.StdIn.readLine("json>")

    if ((program == null) || (json == null)) {
      exit = true
    }

    if (!exit) {
      val parsedprogram = Parser.program.parse(program).get.value
      val results = NewEvaluator.programCompiler(parsedprogram).run(NewEvaluator.State(parse(json).getOrElse(Json.obj()), Json.obj())).unsafeRunSync()
      println(results._1.output)
    }
  }
}
