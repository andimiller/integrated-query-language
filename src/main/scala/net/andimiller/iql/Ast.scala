package net.andimiller.iql

import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

/**
  * Created by andi on 23/12/2015.
  */
object Ast {
  val OM = new ObjectMapper()
  case class World(globals: JsonNode, var output: JsonNode = OM.createObjectNode())
  // structural types
  sealed trait Pipeline
  // reference types
  sealed trait Reference extends Pipeline
  case class Field(path: Seq[String]) extends Reference
  case class OutputField(path: Seq[String]) extends Reference
  case class SettableOutputField(parent: ObjectNode, field: String) extends Reference
  // data types
  sealed trait Data extends Pipeline
  case class Text(value: String) extends Data
  case class Integer(value: Int) extends Data
  case class Float(value: Float) extends Data
  case class Bool(value: Boolean) extends Data
  // containers
  case class Array(values: Seq[Pipeline]) extends Data
  // types
  case object None extends Data
  // operator types
  //    prefix operators
  sealed class PrefixOperator(rhs: Pipeline) extends Pipeline {
    def getRhs = rhs
  }
  case class Not(rhs: Pipeline) extends PrefixOperator(rhs)
  //    infix operators
  sealed class InfixOperator(lhs: Pipeline, rhs: Pipeline) extends Pipeline {
    def getLhs = lhs
    def getRhs = rhs
  }
  case class Equals(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class MoreThan(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class LessThan(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class AND(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class OR(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class XOR(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)
  case class In(lhs: Pipeline, rhs: Pipeline) extends InfixOperator(lhs, rhs)

  // transforms
  sealed trait Transform
  case class Assignment(lhs: OutputField, rhs: Pipeline) extends Transform

  // program
  case class Program(seq: Seq[Assignment])
}
