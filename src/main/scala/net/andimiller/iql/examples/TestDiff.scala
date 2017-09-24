package net.andimiller.iql.examples

import io.circe._
import io.circe.parser._
import net.andimiller.iql.utils.DiffEm
import net.andimiller.iql.Compiler

object TestDiff extends App {

  val a = parse(
    """{
      |  "a": 1,
      |  "b": 2,
      |  "c": 3,
      |  "d": 4
      |}
    """.stripMargin).right.get
  val b = parse(
    """{
      |  "a": 4,
      |  "b": 3,
      |  "c": 2,
      |  "d": 1
      |}
    """.stripMargin).right.get
  val potentialmappings = DiffEm.diff(a, b)
  println(potentialmappings)
  val r = potentialmappings.map(Compiler.programCompiler).map(_.run(Compiler.State.forInput(a)).unsafeRunSync())
  println(r)
}
