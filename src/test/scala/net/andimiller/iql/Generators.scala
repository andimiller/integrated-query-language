package net.andimiller.iql

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.ScalacheckShapeless._

object Generators {
  implicit val fields: Arbitrary[Ast.Field] = Arbitrary {
    for {
      head <- Gen.alphaNumStr
      tail <- Gen.containerOf[List, String](Gen.alphaNumStr)
    } yield (Ast.Field(NonEmptyList(head, tail)))
  }
  implicit val ref       = implicitly[Arbitrary[Ast.Reference]]
  implicit val data      = implicitly[Arbitrary[Ast.Data]]
  implicit val exp       = implicitly[Arbitrary[Ast.Expression]]
  implicit val plus      = implicitly[Arbitrary[Ast.Plus]]
  implicit def shrink[T] = implicitly[Shrink[T]]
}
