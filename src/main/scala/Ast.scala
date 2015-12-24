/**
  * Created by andi on 23/12/2015.
  */
object Ast {
  case class World(globals: Map[String, Any])
  // structural types
  sealed trait Pipeline
  // reference types
  sealed trait Reference extends Pipeline
  case class Field(name: String) extends Reference
  // data types
  sealed trait Data extends Pipeline
  case class Text(value: String) extends Data
  case class Integer(value: Int) extends Data
  case class Float(value: Float) extends Data
  case class Bool(value: Boolean) extends Data
  // types
  case object None extends Data
  // operator types
  sealed class Expression(lhs: Pipeline, rhs: Pipeline)
  case class Equals(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class MoreThan(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  case class LessThan(lhs: Pipeline, rhs: Pipeline) extends Expression(lhs, rhs)
  //case object Not extends Expression
}
