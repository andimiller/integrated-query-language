import com.fasterxml.jackson.databind.JsonNode

/**
  * Created by andi on 23/12/2015.
  */
object Ast {
  case class World(globals: JsonNode)
  // structural types
  sealed trait Pipeline
  // reference types
  sealed trait Reference extends Pipeline
  case class Field(path: Seq[String]) extends Reference
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
  sealed class Expression(lhs: Pipeline, rhs: Pipeline) extends Pipeline {
    def getLhs = lhs
    def getRhs = rhs
  }
  case class Equals(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class MoreThan(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class LessThan(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class AND(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class OR(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class XOR(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class In(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)

}
