package net.andimiller.iql

import org.scalatest.{FlatSpec, MustMatchers}

class UnparserSpec extends FlatSpec with MustMatchers {

  "Unparser" should "unparse a basic expression" in {
    val input = "(1 + 1)"
    val ast   = Parser.bracketedExpression.parse(input).get.value
    Unparser.expressionUnparser(ast) must equal("(1 + 1)")
  }

  "unparser" should "work" in {
    val input = "(0 ^ (((0 == !!false) in .) && ((false < false) + (. > 0))))"
    val ast   = Parser.bracketedExpression.parse(input).get.value
    Unparser.expressionUnparser(ast) must equal(input)
  }
}
