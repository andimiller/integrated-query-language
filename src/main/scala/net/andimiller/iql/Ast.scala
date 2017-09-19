package net.andimiller.iql

import cats.data.NonEmptyList

object Ast {
  // structural types
  sealed trait Expression
  // reference types
  sealed trait Reference                       extends Expression
  case class Field(path: NonEmptyList[String]) extends Reference
  case class OutputField(path: List[String])
  // data types
  sealed trait Data               extends Expression
  case class Text(value: String)  extends Data
  case class Integer(value: Int)  extends Data
  case class Float(value: Double) extends Data
  case class Bool(value: Boolean) extends Data
  // containers
  case class Array(values: List[Expression]) extends Data
  // types
  //case object None extends Data
  // operator types
  //    prefix operators
  sealed class PrefixOperator(rhs: Expression) extends Expression {
    def getRhs = rhs
  }
  case class Not(rhs: Expression) extends PrefixOperator(rhs)
  //    infix operators
  sealed class InfixOperator(lhs: Expression, rhs: Expression) extends Expression {
    def getLhs = lhs
    def getRhs = rhs
  }
  case class Equals(lhs: Expression, rhs: Expression)   extends InfixOperator(lhs, rhs)
  case class MoreThan(lhs: Expression, rhs: Expression) extends InfixOperator(lhs, rhs)
  case class LessThan(lhs: Expression, rhs: Expression) extends InfixOperator(lhs, rhs)
  case class AND(lhs: Expression, rhs: Expression)      extends InfixOperator(lhs, rhs)
  case class OR(lhs: Expression, rhs: Expression)       extends InfixOperator(lhs, rhs)
  case class XOR(lhs: Expression, rhs: Expression)      extends InfixOperator(lhs, rhs)
  case class In(lhs: Expression, rhs: Expression)       extends InfixOperator(lhs, rhs)
  case class Plus(lhs: Expression, rhs: Expression)     extends InfixOperator(lhs, rhs)

  // transforms
  sealed trait Transform
  case class Assignment(lhs: OutputField, rhs: Expression) extends Transform

  case class Validation(lhs: OutputField, rhs: String)

  // program
  case class Program(seq: List[Assignment])
  case class VProgram(seq: List[Validation])
}
