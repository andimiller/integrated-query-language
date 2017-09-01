package net.andimiller.iql

import matryoshka._
import matryoshka.implicits._

import scalaz._
import Scalaz._
import fastparse.all._
import matryoshka.data._

object Calculator extends App {

  // AST

  sealed trait Expr[A]
  final case class Integer[A](value: Int)         extends Expr[A]
  final case class Add[A](left: A, right: A)      extends Expr[A]
  final case class Minus[A](left: A, right: A)    extends Expr[A]
  final case class Multiply[A](left: A, right: A) extends Expr[A]
  final case class Divide[A](left: A, right: A)   extends Expr[A]

  // Algebras

  implicit val calculatorFunctor = new scalaz.Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: (A) => B): Expr[B] = fa match {
      case Integer(value) => Integer[B](value)
      case Add(l, r)      => Add(f(l), f(r))
      case Minus(l, r)    => Minus(f(l), f(r))
      case Multiply(l, r) => Multiply(f(l), f(r))
      case Divide(l, r)   => Divide(f(l), f(r))
    }
  }

  val eval: Algebra[Expr, Int] = {
    case Integer(x)     => x
    case Add(x, y)      => x + y
    case Minus(x, y)    => x - y
    case Multiply(x, y) => x * y
    case Divide(x, y)   => x / y
  }

  // Parser

  import Parser.NamedFunction

  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val digits = P( CharsWhile(Digits))

  val plus     = P("+")
  val minus    = P("-")
  val multiply = P("*")
  val divide   = P("/")

  val symbol = P(plus | minus | multiply | divide)

  def integer[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] = P( P("-").?.! ~ digits.!).map{ x =>
    val (prefix, number) = x
    Integer[T]((prefix+number).toInt).embed
  }

  def expression[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] = P(integer | operatorExpression)

  def operatorExpression[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] = P("(" ~/ expression ~/ symbol.! ~/ expression ~/ ")").map{ e =>
    val (left, symbol, right) = e
    symbol match {
      case "+" => Add[T](left, right).embed
      case "-" => Minus[T](left, right).embed
      case "*" => Multiply[T](left, right).embed
      case "/" => Divide[T](left, right).embed
    }
  }

  def calculator[T](implicit T: Corecursive.Aux[T, Expr]): Parser[T] = P(operatorExpression ~ End)

  // and usage

  val r =  calculator[Nu[Expr]].parse("((((-1+2)+4)+8)*2)").get.value.cata(eval)
  println(r)

}
