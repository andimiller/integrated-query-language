import fastparse.core.Parsed.Success
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by andi on 23/12/2015.
  */
class EvaluatorSpec extends FlatSpec with MustMatchers {

  import Evaluator._

  "Evaluating a reference" should "fetch the value from the global variable map" in {
    val world = new Ast.World(Map("foo" -> "fooval"))
    Parser.reference.parse("foo") match {
      case Success(v, i) =>
        v.asInstanceOf[Ast.Reference].eval(world) must equal(Ast.Text("fooval"))
    }
  }

  "Evaluating an equality that's not true" should "return false" in {
    Parser.Equality.parse("\"foo\"=\"nope\"") match {
      case Success(v, i) =>
        v.eval(new Ast.World(Map())) must equal(Ast.Bool(false))
    }
  }

  "Evaluating an equality that's true" should "return true" in {
    Parser.Equality.parse("\"foo\"=\"foo\"") match {
      case Success(v, i) =>
        v.eval(new Ast.World(Map())) must equal(Ast.Bool(true))
    }
  }

  "Evaluating a lessthan that's true" should "return true" in {
    Parser.LessThan.parse("4<42") match {
      case Success(v, i) =>
        v.eval(new Ast.World(Map())) must equal(Ast.Bool(true))
    }
  }
}
