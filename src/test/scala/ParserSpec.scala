import fastparse.core.Parsed.{Failure, Success}
import org.scalatest.{MustMatchers, FlatSpec}

/**
  * Created by andi on 23/12/2015.
  */
class ParserSpec extends FlatSpec with MustMatchers {

  "Strings" should "only be valid if they have double quotes on both ends" in {
    val inputs = Map(
      "\"hello world" -> false,
      "\"hello world\"" -> true,
      "boo" -> false,
      "boo\"" -> false,
      "\"BOO\"" -> true
    )

    inputs.foreach { i =>
      val (input, expected) = i
      val result = Parser.string.parse(input)
      expected match {
        case true => result.isInstanceOf[Success[String]] must equal(true)
        case false => result.isInstanceOf[Failure] must equal(true)
      }
    }
  }

  "Strings" should "have their double quotes stripped" in {
    val input = "\"hello world\""
    Parser.string.parse(input) must equal(Success(Ast.Text(input.replace("\"", "")), input.length))
  }


  "References" should "be valid if they don't use special characters" in {
    val input = "reference"
    Parser.reference.parse(input) must equal(Success(Ast.Field("reference"), input.length))
  }

  "References" should "not be valid with quotes" in {
    val input = "\"hello"
    val r = Parser.reference.parse(input)
    r.isInstanceOf[Failure] must equal(true)
  }

  "References" should "cut off after it looks valid" in {
    val input = "foo bar"
    val r = Parser.reference.parse(input)
    println(r)
  }

  "Equality statements" should "be valid with two references" in {
    val input = "foo=bar"
    Parser.Equality.parse(input) must equal(Success(Ast.Equals(Ast.Field("foo"), Ast.Field("bar")), input.length))
  }

  "Equality statements" should "be valid with two references and whitespace" in {
    val input = "foo = bar"
    Parser.Equality.parse(input) must equal(Success(Ast.Equals(Ast.Field("foo"), Ast.Field("bar")), input.length))
  }


}
