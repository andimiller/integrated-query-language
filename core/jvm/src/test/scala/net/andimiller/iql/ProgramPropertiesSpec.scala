package net.andimiller.iql

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, MustMatchers}
//import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest._
import Generators._

class ProgramPropertiesSpec extends FlatSpec with Checkers with Matchers {
  /*
  "Plus" should "be completely defined" in {
    var c = 0L
    import Compiler._
    check { (p: Ast.Plus) =>
      c += 1
      println(c)
      infixCompiler(p)
        .run(State.empty)
        .attempt
        .unsafeRunSync()
        .isRight // as long as it didn't exception we're good
    }
  }
  "Expression" should "be unparseable then parseable" in {
    check { (e: Ast.Expression) =>
      val textual = Unparser.expressionUnparser(e)
      println("====== AST")
      println(e)
      println("=== original")
      println(textual)
      val ast = Parser.Expression.parse(textual).get.value
      println("||| reparsed")
      println(ast)
      if (ast == e) {
        true
      } else {
        import ai.x.diff.DiffShow
        import ai.x.diff.conversions._
        println(DiffShow.diff[Ast.Expression](e, ast).string)
        false
      }
    }
  }
  */
}
