package net.andimiller.iql

import scala.util.Try

import cats.syntax._
import cats.implicits._
import cats._
import cats.data._

object Parser {
  import fastparse.all._

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T)         = f(t)
    override def toString() = name
  }

  // basics
  val Whitespace  = NamedFunction(" \n".contains(_: Char), "Whitespace")
  val Digits      = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space     = P(CharsWhile(Whitespace).?)
  val digits    = P(CharsWhile(Digits))
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val letter    = P(lowercase | uppercase)
  val equals    = P("=")
  val is        = P(":")

  val hexDigit      = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  // data structures
  val array =
    P("[" ~/ Expression.? ~ ("," ~ space.? ~ Expression).rep.? ~ "]")
      .map { case (a, b) => a.toList ++ b.toList.flatten }
      .map(Ast.Array)

  // types
  val strChars       = P(CharsWhile(StringChars))
  val string         = P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Ast.Text)
  val wildcard       = P("*")
  val squarebrackets = P("[" | "]")
  val referenceChars = P(letter | digits | wildcard | squarebrackets)
  val reference = P(&(".") ~/ ("." ~/ referenceChars.rep.!) ~/ ("." ~/ referenceChars.rep.!).rep).map {
    case (h, t) => Ast.Field.apply(NonEmptyList(h, t.toList))
  }
  val outputReferenceChars = P(letter | digits)
  val outputReference =
    P(&(".") ~/ ("." ~/ outputReferenceChars.rep.!).rep).map(t => Ast.OutputField.apply(t.toList))
  val number =
    P("-".? ~ digits ~ digits.rep ~ !".").!.map(s => Ast.Integer(Integer.parseInt(s.toString)))
  val float =
    P("-".? ~ P(digits ~ digits.rep) ~ "." ~ P(digits ~ digits.rep) ~ P("E" ~ "-".? ~ digits ~ digits.rep).?).!.map { s =>
      Ast.Float(Try { s.toDouble }.getOrElse(0.0d))
    }
  val boolean = P("true" | "false").!.map(_ match {
    case "true"  => Ast.Bool(true)
    case "false" => Ast.Bool(false)
  })

  // Nots
  val Notted = P("!" ~ space.? ~/ Expression).map(Ast.Not)

  // code
  val Expression: Parser[Ast.Expression] = P(Notted | number | float | string | reference | boolean | array | bracketedExpression)
  val OperatorExpression: Parser[Ast.Expression] =
    P(Expression ~/ space.? ~/ (("==" | "<" | ">" | "&&" | "||" | "^" | "in" | "+" | "|").! ~/ space.? ~/ Expression).rep(min = 1))
      .map {
        case (l, exps) =>
          exps.foldLeft(l) { case (acc, (operator, exp)) =>
            operator match {
              case "==" => Ast.Equals(acc, exp)
              case "<"  => Ast.LessThan(acc, exp)
              case ">"  => Ast.MoreThan(acc, exp)
              case "&&" => Ast.AND(acc, exp)
              case "||" => Ast.OR(acc, exp)
              case "^"  => Ast.XOR(acc, exp)
              case "in" => Ast.In(acc, exp)
              case "+"  => Ast.Plus(acc, exp)
              case "|"  => Ast.Coalesce(acc, exp)
            }
          }
      }
  val bracketedExpression: Parser[Ast.Expression] = P("(" ~/ OperatorExpression ~ ")")
  val toplevelExpression: Parser[Ast.Expression]     = P(P(Expression ~ newline) | P(OperatorExpression ~ newline))

  val function = P("required" | "int" | "bool" | "string").!

  // transforms and validation
  val assignment =
    P(outputReference ~ space.? ~ equals ~/ space.? ~ toplevelExpression)
      .map(Ast.Assignment.tupled)
  val validation = P(outputReference ~ space.? ~ is ~ space.? ~/ function)
    .map(Ast.Validation.tupled)

  // full programs
  val newline = P("\n" | "\r\n" | "\r" | "\f" | End)
  val program = P(assignment.rep).map(t => Ast.Program(t.toList))
  val validationProgram =
    P(validation ~ newline ~/ (validation ~/ newline).rep).map { case (v, vs) => Ast.VProgram(NonEmptyList(v, vs.toList)) }
}
