/**
  * Created by andi on 23/12/2015.
  */
object Ast {
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
  case class Boolean(value: Boolean) extends Data
  // operator types
  sealed trait InfixOperator
  sealed trait PostfixOperator
  sealed trait PrefixOperator
  case class Equals(lhs: Pipeline, rhs: Pipeline) extends InfixOperator
  case object BiggerThan extends InfixOperator
  case object LessThan extends InfixOperator
  case object Not extends PrefixOperator
}
