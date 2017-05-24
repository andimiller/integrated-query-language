package net.andimiller.iql

import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

/**
  * Created by andi on 23/12/2015.
  */
object NewAst extends App {
  val OM = new ObjectMapper()
  case class World(globals: JsonNode, var output: JsonNode = OM.createObjectNode())
  // structural types
  sealed trait Pipeline[P]
  // reference types
  sealed trait Reference[R] extends Pipeline[R]
  case class Field[R](path: Seq[String]) extends Reference[R]
  case class OutputField[R](path: Seq[String]) extends Reference[R]
  case class SettableOutputField[R](parent: ObjectNode, field: String) extends Reference[R]
  // data types
  sealed trait Data[D] extends Pipeline[D]
  case class Text[D](value: String) extends Data[D]
  case class Integer[D](value: Int) extends Data[D]
  case class Float[D](value: scala.Float) extends Data[D]
  case class Bool[D](value: Boolean) extends Data[D]
  // containers
  case class Array[D](values: Seq[Pipeline[D]]) extends Data[D]
  // types
  case class None[P]() extends Data[P]
  // operator types
  //    prefix operators
  sealed class PrefixOperator[P](rhs: Pipeline[P]) extends Pipeline[P] {
    def getRhs = rhs
  }
  case class Not[P](rhs: Pipeline[P]) extends PrefixOperator[P](rhs)
  //    infix operators
  sealed class InfixOperator[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends Pipeline[P] {
    def getLhs = lhs
    def getRhs = rhs
  }
  case class Equals[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class MoreThan[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class LessThan[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class AND[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class OR[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class XOR[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)
  case class In[P](lhs: Pipeline[P], rhs: Pipeline[P]) extends InfixOperator[P](lhs, rhs)

  // transforms
  sealed trait Transform[T]
  case class Assignment[T](lhs: OutputField[T], rhs: Pipeline[T]) extends Transform[T]

  // program
  case class Program[P](seq: Seq[Assignment[P]])

}
