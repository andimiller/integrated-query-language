import com.fasterxml.jackson.databind.ObjectMapper
import fastparse.core.Parsed.Success
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by andi on 23/12/2015.
  */
class EvaluatorSpec extends FlatSpec with MustMatchers {

  import Evaluator._

  val OM = new ObjectMapper()

  "Evaluating a reference" should "fetch the value from the global variable map" in {
    val world = new Ast.World(OM.readTree("{\"foo\": \"fooval\"}"))
    Parser.reference.parse(".foo") match {
      case Success(v, i) =>
        v.asInstanceOf[Ast.Reference].eval(world) must equal(Ast.Text("fooval"))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating a non-existent reference" should "fail gracefully" in {
    val world = new Ast.World(OM.readTree("{\"foo\": \"fooval\"}"))
    Parser.reference.parse(".bar") match {
      case Success(v, i) =>
        v.asInstanceOf[Ast.Reference].eval(world) must equal(Ast.None)
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating a non-existent chained reference" should "fail gracefully" in {
    val world = new Ast.World(OM.readTree("{\"foo\": \"fooval\"}"))
    Parser.reference.parse(".bar.baz.foo") match {
      case Success(v, i) =>
        v.asInstanceOf[Ast.Reference].eval(world) must equal(Ast.None)
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating a non-existent chained reference in an equality check" should "fail gracefully" in {
    val world = new Ast.World(OM.readTree("{\"foo\": \"fooval\"}"))
    Parser.OperatorExpression.parse(".bar.baz.foo == \"moo\"") match {
      case Success(v, i) =>
        v.eval(world) must equal(Ast.Bool(false))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating an equality that's not true" should "return false" in {
    Parser.OperatorExpression.parse("\"foo\"==\"nope\"") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.createObjectNode())) must equal(Ast.Bool(false))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating an equality that's true" should "return true" in {
    Parser.OperatorExpression.parse("\"foo\"==\"foo\"") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.createObjectNode())) must equal(Ast.Bool(true))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating an equality that's NOT true" should "return true" in {
    Parser.Expression.parse("!(\"foo\"==\"foo\")") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.createObjectNode())) must equal(Ast.Bool(false))
      case other => fail("unable to parse query")
    }
  }

  "Evaluating a lessthan that's true" should "return true" in {
    Parser.OperatorExpression.parse("4<42") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.createObjectNode())) must equal(Ast.Bool(true))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating a multi-level OR that's true" should "return true" in {
    Parser.OperatorExpression.parse("(1==1)||(2==2)") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.createObjectNode())) must equal(Ast.Bool(true))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating multi-level AND in a production-style manner" should "function correctly" in {
    val inputJson =
      """ {
        | "favouriteAnimal": "cat",
        | "numberOfAnimals": 3
        |}
      """.stripMargin
    val inputFilter =
      """(.favouriteAnimal == "cat") && (.numberOfAnimals == 3) """
    Parser.OperatorExpression.parse(inputFilter) match {
      case Success(exp, count) =>
        val result = exp.eval(new Ast.World(OM.readTree(inputJson)))
        result must equal(Ast.Bool(true))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating multi-level AND with a MoreThan in a production-style manner" should "function correctly" in {
    val inputJson =
      """ {
        | "favouriteAnimal": "cat",
        | "numberOfAnimals": 3
        |}
      """.stripMargin
    val inputFilter =
      """(.favouriteAnimal == "cat") && (.numberOfAnimals > 1) """
    Parser.program.parse(inputFilter) match {
      case Success(exp, count) =>
        val result = exp.eval(new Ast.World(OM.readTree(inputJson)))
        result must equal(Ast.Bool(true))
        val inputJson2 = inputJson.replace("3", "0")
        val result2 = exp.eval(new Ast.World(OM.readTree(inputJson2)))
        result2 must equal(Ast.Bool(false))
      case _ => fail("unable to parse query")
    }
  }

  "Evaluating multi-level AND with a MoreThan over multi-level JSON in a production-style manner" should "function correctly" in {
    val inputJson =
      """ {
        |   "data": {
        |    "favouriteAnimal": "cat",
        |     "numberOfAnimals": 3
        |   }
        |}
      """.stripMargin
    val inputFilter =
      """(.data.favouriteAnimal == "cat") && (.data.numberOfAnimals > 1) """
    Parser.program.parse(inputFilter) match {
      case Success(exp, count) =>
        val result = exp.eval(new Ast.World(OM.readTree(inputJson)))
        result must equal(Ast.Bool(true))
        val inputJson2 = inputJson.replace("3", "0")
        val result2 = exp.eval(new Ast.World(OM.readTree(inputJson2)))
        result2 must equal(Ast.Bool(false))
      case _ => fail("unable to parse query")
    }
  }


  "Evaluating a containment that's true" should "return true" in {
    val json =
      """{
        |  "data": [1, 2, 3, 4, 5]
        |}
      """.stripMargin
    Parser.OperatorExpression.parse("5 in .data") match {
      case Success(v, i) =>
        v.eval(new Ast.World(OM.readTree(json))) must equal(Ast.Bool(true))
      case _ => fail("unable to parse query")
    }
  }
}
