package net.andimiller.iql

import fastparse.core.Parsed.{Failure, Success}
import org.scalatest.{FlatSpec, MustMatchers}

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
        case true => result.isInstanceOf[Success[_, _, _]] must equal(true)
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
    val r = Parser.number.parse(input)
    r must equal(Success(Ast.Integer(42), input.length))
  }

  "References" should "be valid if they don't use special characters" in {
    val input = ".reference"
    Parser.reference.parse(input) must equal(Success(Ast.Field(Seq("reference")), input.length))
  }

  "References" should "cut off a weird special characters" in {
    val input = ".\"hello"
    val r = Parser.reference.parse(input)
    r must equal(Success(Ast.Field(Seq()), 1))
  }

  "References" should "cut off after it looks valid" in {
    val input = ".foo .bar"
    val marker = input.indexOf(" ")
    Parser.reference.parse(input) must equal(Success(Ast.Field(Seq(input.substring(0, marker).stripPrefix("."))), marker))
  }

  "Equality statements" should "be valid with two references" in {
    val input = ".foo==.bar"
    Parser.OperatorExpression.parse(input) must equal(Success(Ast.Equals(Ast.Field(Seq("foo")), Ast.Field(Seq("bar"))), input.length))
  }

  "Equality statements" should "be valid with two references and whitespace" in {
    val input = ".foo == .bar"
    Parser.OperatorExpression.parse(input) must equal(Success(Ast.Equals(Ast.Field(Seq("foo")), Ast.Field(Seq("bar"))), input.length))
  }

  "Arrays" should "happen" in {
    val input = "[1,2,3]"
    Parser.array.parse(input) must equal(Success(Ast.Array(Seq(Ast.Integer(1), Ast.Integer(2), Ast.Integer(3))), input.length))
  }

  "Arrays" should "happen as expressions" in {
    val input = "[1,2,3]"
    Parser.Expression.parse(input) must equal(Success(Ast.Array(List(1,2,3).map(Ast.Integer)), input.length))
  }

  "Booleans" should "happen" in {
    Parser.boolean.parse("true") must equal(Success(Ast.Bool(true), 4))
    Parser.boolean.parse("false") must equal(Success(Ast.Bool(false), 5))
  }

  "BiggerThan" should "parse correctly via Expression" in {
    val input = ".cats < 10"
    val r = Parser.Expression.parse(input)

  }

}
