package net.andimiller.iql

import fastparse.core.Parsed.Success
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by andi on 26/12/2015.
  */
class RealisticEvaluatorSpec extends FlatSpec with MustMatchers {

  import Evaluator._
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
        val r = v.eval(OM.readTree(inputjson))
        r.toString must equal("""{"a":{"value":"a","b":{"c":"nested"},"original":"originala"},"b":"b"}""")
      case f =>
        fail("unable to parse query")
    }
  }

}
