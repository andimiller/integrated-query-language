/**
  * Created by andi on 23/12/2015.
  */
object Ast {
  case class World(globals: Map[String, Any])
  sealed trait AnyVal
  // structural types
  sealed trait Pipeline
  // reference types
  sealed trait Reference extends Pipeline with AnyVal
  case class Field(name: String) extends Reference
  // data types
  sealed trait Data extends Pipeline with AnyVal
  case class Text(value: String) extends Data
  case class Integer(value: Int) extends Data
  case class Float(value: Float) extends Data
  case class Bool(value: Boolean) extends Data
  // types
  case object None extends Data
  // operator types
  sealed trait Expression
  case class Equals(lhs: Pipeline, rhs: Pipeline) extends Expression
  case object BiggerThan extends Expression
  case object LessThan extends Expression
  //case object Not extends Expression
}
