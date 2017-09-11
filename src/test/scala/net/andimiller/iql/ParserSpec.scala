package net.andimiller.iql

import fastparse.core.Parsed.{Failure, Success}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by andi on 23/12/2015.
  */
class ParserSpec extends FlatSpec with MustMatchers {

  "Strings" should "only be valid if they have double quotes on both ends" in {
    val inputs = Map(
      "\"hello world"   -> false,
      "\"hello world\"" -> true,
      "boo"             -> false,
      "boo\""           -> false,
      "\"BOO\""         -> true
    )

    inputs.foreach { i =>
      val (input, expected) = i
      val result            = Parser.string.parse(input)
      expected match {
        case true  => result.isInstanceOf[Success[_, _, _]] must equal(true)
        case false => result.isInstanceOf[Failure[_, _]] must equal(true)
      }
    }
  }

  "Strings" should "have their double quotes stripped" in {
    val input = "\"hello world\""
    Parser.string.parse(input) must equal(Success(Ast.Text(input.replace("\"", "")), input.length))
  }

  "Integers" should "parse correctly" in {
    val input = "42"
    val r     = Parser.number.parse(input)
    r must equal(Success(Ast.Integer(42), input.length))
  }

  "References" should "be valid if they don't use special characters" in {
    val input = ".reference"
    Parser.reference.parse(input) must equal(Success(Ast.Field(List("reference")), input.length))
  }

  "References" should "cut off a weird special characters" in {
    val input = ".\"hello"
    val r     = Parser.reference.parse(input)
    r must equal(Success(Ast.Field(List("")), 1))
  }

  "References" should "cut off after it looks valid" in {
    val input  = ".foo .bar"
    val marker = input.indexOf(" ")
    Parser.reference.parse(input) must equal(Success(Ast.Field(List(input.substring(0, marker).stripPrefix("."))), marker))
  }

  "Equality statements" should "be valid with two references" in {
    val input = ".foo==.bar"
    Parser.OperatorExpression.parse(input) must equal(Success(Ast.Equals(Ast.Field(List("foo")), Ast.Field(List("bar"))), input.length))
  }

  "Equality statements" should "be valid with two references and whitespace" in {
    val input = ".foo == .bar"
    Parser.OperatorExpression.parse(input) must equal(Success(Ast.Equals(Ast.Field(List("foo")), Ast.Field(List("bar"))), input.length))
  }

  "Arrays" should "happen" in {
    val input = "[1,2,3]"
    Parser.array.parse(input) must equal(Success(Ast.Array(List(Ast.Integer(1), Ast.Integer(2), Ast.Integer(3))), input.length))
  }

  "Arrays" should "happen as expressions" in {
    val input = "[1,2,3]"
    Parser.Expression.parse(input) must equal(Success(Ast.Array(List(1, 2, 3).map(Ast.Integer)), input.length))
  }

  "Booleans" should "happen" in {
    Parser.boolean.parse("true") must equal(Success(Ast.Bool(true), 4))
    Parser.boolean.parse("false") must equal(Success(Ast.Bool(false), 5))
  }

  "BiggerThan" should "parse correctly via Expression" in {
    val input = ".cats < 10"
    val r     = Parser.Expression.parse(input)

  }

  "Plus" should "parse correctly via OperatorExpression" in {
    val input = "2 + 2"
    val r     = Parser.OperatorExpression.parse(input)
    r must equal(Success(Ast.Plus(Ast.Integer(2), Ast.Integer(2)), input.length))
  }

  "Plus" should "parse correctly via Assignment" in {
    val input = ".b = 2 + 2"
    val r     = Parser.assignment.parse(input)
    r must equal(Success(Ast.Assignment(Ast.OutputField(List("b")), Ast.Plus(Ast.Integer(2), Ast.Integer(2))), input.length))
  }

  "A two line program" should "parse correctly via program" in {
    val input =
      """.a = "a"
        |.b = "b"""".stripMargin
    val r = Parser.program.parse(input)
    import Ast._
    r must equal(
      Success(Program(List(Assignment(OutputField(List("a")), Text("a")), Assignment(OutputField(List("b")), Text("b")))), input.length))
  }

  "A two line program with an operator expression" should "parse correctly via program" in {
    val input =
      """.a = "a"
        |.b = 2 + 2""".stripMargin
    val r = Parser.program.parse(input)
    import Ast._
    r must equal(
      Success(Program(List(Assignment(OutputField(List("a")), Text("a")), Assignment(OutputField(List("b")), Plus(Integer(2), Integer(2))))),
              input.length))
  }

  "Float" should "parse 0.0" in {
    val input = "0.0"
    val r = Parser.float.parse(input)
    r must equal(Success(Ast.Float(0.0d), input.size))
  }
}
