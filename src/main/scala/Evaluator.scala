import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

import scala.collection.JavaConverters._

/**
  * Created by andi on 23/12/2015.
  */
object Evaluator {
  val OM = new ObjectMapper()

  class TypeMappingFailed extends Exception
  class UnsupportedFunctionArgumentTypes extends Exception
  class MaximumEvalStackDepth extends Exception

  trait Evaluator[A] {
    def eval(world: Ast.World): Ast.Pipeline
  }

  def anyToAst(a: Any): Option[Ast.Pipeline] = {
    a match {
      case s: String => Some(Ast.Text(s))
      case i: Integer => Some(Ast.Integer(i))
      case _ => throw new TypeMappingFailed
    }
  }

  def jsonNodeToAst(j: JsonNode): Option[Ast.Pipeline] = {
    j match {
      case i if i.isInt => Some(Ast.Integer(i.asInt()))
      case s if s.isTextual => Some(Ast.Text(s.asText()))
      case b if b.isBoolean => Some(Ast.Bool(b.asBoolean()))
      case a if a.isArray =>
        val array = a.elements().asScala.map(jsonNodeToAst).flatten.toSeq
        Some(Ast.Array(array))
    }
  }

  implicit class EvaluatableReference(r: Ast.Reference) extends Evaluator[Ast.Reference] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      r match {
        case f: Ast.Field =>
          val resolved = f.path.foldLeft(world.globals)((node, path) => node.get(path))
          jsonNodeToAst(resolved).getOrElse(Ast.None)
      }
    }
  }


  implicit class EvaluatablePipeline(p: Ast.Pipeline) extends Evaluator[Ast.Pipeline] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      p match {
        case r: Ast.Reference => r.eval(world)
        case d: Ast.Data => d
        case e: Ast.Expression => e.eval(world)
      }
    }
  }


  implicit class EvaluatableExpression(e: Ast.Expression) extends Evaluator[Ast.Expression] {

    def resolveUntilData(t: Ast.Pipeline, limit: Int = 250)(world: Ast.World): Ast.Data = {
      val r = t.eval(world)
      if (limit == 0) {
        throw new MaximumEvalStackDepth
      }
      r match {
        case d: Ast.Data => d
        case p: Ast.Pipeline => resolveUntilData(p, limit-1)(world)
      }
    }

    override def eval(world: Ast.World): Ast.Pipeline = {
      val lhs = resolveUntilData(e.getLhs)(world)
      val rhs = resolveUntilData(e.getRhs)(world)
      e match {
        case equals: Ast.Equals => Ast.Bool(lhs == rhs)
        case lessthan: Ast.LessThan =>
          (lhs, rhs) match {
            case (l: Ast.Integer, r: Ast.Integer) => Ast.Bool(l.value < r.value)
            case _ =>
              throw new UnsupportedOperationException
          }

        case morethan: Ast.MoreThan =>
          (lhs, rhs) match {
            case (l: Ast.Integer, r: Ast.Integer) => Ast.Bool(l.value > r.value)
            case _ => throw new UnsupportedOperationException
          }

        case or: Ast.OR =>
          (lhs, rhs) match {
            case (l: Ast.Bool, r: Ast.Bool) => Ast.Bool(l.value || r.value)
            case _ => throw new UnsupportedOperationException
          }

      }
    }
  }

}
