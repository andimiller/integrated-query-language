package net.andimiller.iql.utils


import io.circe._
import io.circe.syntax._
import net.andimiller.iql.Ast
import cats._, cats.syntax._, cats.instances._, cats.implicits._, cats.data._

object DiffEm {

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List.empty[List[T]]
    case head :: Nil  =>  List(head)
    case head :: tail => for (xh <- head; xt <- cartesianProduct(tail)) yield xh :: xt
  }


  def diff(a: Json, b: Json): List[Ast.Program] = {
    // flattened
    val fa = Flatten.flatten(a)
    val fb = Flatten.flatten(b)
    // map
    val fam = fa.asObject.get.toMap
    val fbm = fb.asObject.get.toMap
    // reverse map
    val famr = fam.toList.groupBy(_._2).map { case (k, vs) => (k, vs.map(_._1))}
    val fbmr = fbm.toList.groupBy(_._2).map { case (k, vs) => (k, vs.map(_._1))}
    // let's go
    val intersection = famr.keySet intersect fbmr.keySet
    val mappings = intersection.map { v =>
      val from = famr.getOrElse(v, List.empty[String])
      val to   = fbmr.getOrElse(v, List.empty[String])
      (from |@| to) map { case (in, out) =>
        Ast.Assignment(
          Ast.OutputField(out.stripPrefix(".").split('.').toList),
          Ast.Field(NonEmptyList.fromListUnsafe(in.stripPrefix(".").split('.').toList))
        )
      }
    }.toList
    // generate every program
    cartesianProduct(mappings).map(Ast.Program)
  }

}
