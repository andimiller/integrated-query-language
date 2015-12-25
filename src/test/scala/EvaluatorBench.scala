import fastparse.core.Parsed.Success
import org.scalameter._

/**
  * Created by andi on 24/12/2015.
  */
object EvaluatorBench extends App {
  val world = new Ast.World(Map("foo" -> "bar"))
  val world2 = new Ast.World(Map("foo" -> "baz"))

  val benchConfig = config(
    Key.exec.benchRuns -> 100000
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  }

  val parseTime = benchConfig measure {
    Parser.Equality.parse("foo=bar")
  }
  println(s"parser took $parseTime")
  val filter = Parser.Equality.parse("foo=bar") match {
    case Success(e, c) =>
      import Evaluator._
      val evalTime = benchConfig measure {
        e.eval(world)
      }
      println(s"eval on a matching target took $evalTime")
      val evalTime2 = benchConfig measure {
        e.eval(world2)
      }
      println(s"eval on a non-matching target took $evalTime2")

  }

}
