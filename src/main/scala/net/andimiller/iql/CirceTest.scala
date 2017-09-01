package net.andimiller.iql
import io.circe._, io.circe.parser._

object CirceTest extends App {
   val program = """.a.value = "a";.b = "b";.a.b.c = "nested";.a.original = .a""".stripMargin
    val inputjson =
      """{
        | "a": "originala"
        |}""".stripMargin

  val parsedprogram = Parser.program.parse(program).get.value
  val results = NewEvaluator.ProgramCompiler.compile(parsedprogram).run(NewEvaluator.State(parse(inputjson).getOrElse(Json.obj()), Json.obj())).unsafeRunSync()
  println(results)
}
