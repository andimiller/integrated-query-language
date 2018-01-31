package net.andimiller.iql

import fastparse.core.Parsed.Success
import org.scalatest.{FlatSpec, MustMatchers}
import io.circe._, io.circe.parser._

/**
  * Created by andi on 26/12/2015.
  */
class RealisticEvaluatorSpec extends FlatSpec with MustMatchers {

  "Evaluating a multi-line program" should "return the right constructed JSON object" in {
    val program =
      """.a.value = "a"
        |.b = "b"
        |.a.b.c = "nested"
        |.a.original = .a""".stripMargin
    val inputjson =
      """{
        | "a": "originala"
        |}
      """.stripMargin
    Parser.program.parse(program) match {
      case Success(v, i) =>
        val result = Compiler
          .programCompiler(v)
          .run(Compiler.State(parse(inputjson).right.toOption.getOrElse(Json.obj()), Json.obj()))
        result.unsafeRunSync().output.noSpaces must equal("""{"a":{"value":"a","b":{"c":"nested"},"original":"originala"},"b":"b"}""")
      case f =>
        println(f)
        fail("unable to parse query")
    }
  }

  "Evaluating a multi-line program" should "return the right constructed JSON object, round 2" in {
    val program =
      """.a.value = "a"
        |.b = 2 + 2
        |.a.b.c = "nested"
        |.a.original = .a""".stripMargin
    val inputjson =
      """{
        | "a": "originala"
        |}
      """.stripMargin
    Parser.program.parse(program) match {
      case Success(v, i) =>
        val result = Compiler
          .programCompiler(v)
          .run(Compiler.State(parse(inputjson).right.toOption.getOrElse(Json.obj()), Json.obj()))
        result.unsafeRunSync().output.noSpaces must equal("""{"a":{"value":"a","b":{"c":"nested"},"original":"originala"},"b":4}""")
      case f =>
        println(f)
        fail("unable to parse query")
    }
  }

  "Evaluating a multi-line program with match blocks" should "work" in {
    val program =
      """.result = match a
| 1 => "one"
| 2 => "two"
"""

    val inputjson =
      """{
        | "a": "2"
        |}
      """.stripMargin
    Parser.program.parse(program) match {
      case Success(v, i) =>
        val result = Compiler
          .programCompiler(v)
          .run(Compiler.State(parse(inputjson).right.toOption.getOrElse(Json.obj()), Json.obj()))
        result.unsafeRunSync().output.noSpaces must equal("""{"a":{"value":"a","b":{"c":"nested"},"original":"originala"},"b":4}""")
      case f =>
        println(f)
        fail("unable to parse query")
    }
  }
}
