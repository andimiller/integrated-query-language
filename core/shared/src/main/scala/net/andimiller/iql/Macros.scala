package net.andimiller.iql

import cats._, cats.implicits._, cats.data._, cats.syntax._
import Ast._

object Macros {

  case class Folder(
      f1: Program => Program = identity,
      f2: Assignment => Assignment = identity,
      f3: OutputField => OutputField = identity,
      f4: Expression => Expression = identity
  )

  def fold(p: Program)(f: Folder) = {
    f.f1(p.copy(seq = p.seq.map { a =>
      f.f2(a.copy(lhs = f.f3(a.lhs), rhs = f.f4(a.rhs)))
    }))
  }

  case class ExpressionFolder(
                             r1: Reference => Reference,
                             d1: Data => Data,
                             p1: PrefixOperator => PrefixOperator,
                             i1: InfixOperator => InfixOperator
                             )

  def expressionFolder() = (e: Expression) => e match {
    case r: Reference =>
    case d: Data =>
    case p: PrefixOperator =>
    case i: InfixOperator =>
  }

}
