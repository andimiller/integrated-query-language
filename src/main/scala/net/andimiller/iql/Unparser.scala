package net.andimiller.iql

import cats.data._
import cats.effect._

object Unparser {
  type Unparser[T] = T => String

  val referenceUnparser: Unparser[Ast.Reference] = (t: Ast.Reference) => {
    t match {
      case Ast.Field(path) =>
        "."+path.mkString(".")
    }
  }

  val expressionUnparser: Unparser[Ast.Expression] = (t: Ast.Expression) =>
      t match {
        case r: Ast.Reference => referenceUnparser(r)
        case d: Ast.Data =>
          d match {
            case t: Ast.Text    => "\"" + t.value + "\""
            case i: Ast.Integer => i.value.toString
            case f: Ast.Float   => f.value.toString
            case b: Ast.Bool    => b.value.toString
            case a: Ast.Array   => "["+a.values.map(expressionUnparser).mkString(",")+"]"
          }
        case e: Ast.InfixOperator   => infixUnparser(e)
        case pe: Ast.PrefixOperator => prefixUnparser(pe)
      }

  val infixUnparser: Unparser[Ast.InfixOperator] = (t: Ast.InfixOperator) => {
    val lhs = expressionUnparser(t.getLhs)
    val rhs = expressionUnparser(t.getRhs)
    val symbol = t match {
      case _: Ast.Equals => "=="
      case _: Ast.LessThan => "<"
      case _: Ast.MoreThan => ">"
      case _: Ast.OR => "||"
      case _: Ast.AND => "&&"
      case _: Ast.In => "in"
      case _: Ast.XOR => "^"
      case _: Ast.Plus => "+"
    }
    s"($lhs $symbol $rhs)"
  }

  val prefixUnparser: Unparser[Ast.PrefixOperator] = (t: Ast.PrefixOperator) => {
    val rhs = expressionUnparser(t.getRhs)
    val symbol = t match {
      case _: Ast.Not => "!"
    }
    s"$symbol$rhs"
  }

  val outputFieldUnparser: Unparser[Ast.OutputField] = (t: Ast.OutputField) => {
    "+"+t.path.mkString(".")
  }

  val assignmentUnparser: Unparser[Ast.Assignment] = (t: Ast.Assignment) => {
    val lhs = outputFieldUnparser(t.lhs)
    val rhs = expressionUnparser(t.rhs)
    s"$lhs = $rhs"
  }

  val validationUnparser: Unparser[Ast.Validation] = (t: Ast.Validation) => {
    val lhs = outputFieldUnparser(t.lhs)
    val rhs = t.rhs
    s"$lhs : $rhs"
  }

  val programUnparser: Unparser[Ast.Program] = (t: Ast.Program) => {
    t.seq.map(assignmentUnparser).mkString("\n")
  }

  val vprogramUnparser: Unparser[Ast.VProgram] = (t: Ast.VProgram) => {
    t.seq.map(validationUnparser).mkString("\n")
  }
}
