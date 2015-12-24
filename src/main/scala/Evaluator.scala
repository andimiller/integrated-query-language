/**
  * Created by andi on 23/12/2015.
  */
object Evaluator {
  class TypeMappingFailed extends Exception
  class UnsupportedFunctionArgumentTypes extends Exception
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

  implicit class EvaluatableReference(r: Ast.Reference) extends Evaluator[Ast.Reference] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      r match {
        case f: Ast.Field =>
          world.globals.get(f.name).flatMap(anyToAst).getOrElse(Ast.None)
      }
    }
  }


  implicit class EvaluatablePipeline(p: Ast.Pipeline) extends Evaluator[Ast.Pipeline] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      p match {
        case r: Ast.Reference => r.eval(world)
        case d: Ast.Data => d
      }
    }
  }


  implicit class EvaluatableExpression(e: Ast.Expression) extends Evaluator[Ast.Expression] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      e match {
        case equals: Ast.Equals => Ast.Bool(equals.lhs.eval(world) == equals.rhs.eval(world))
        case lessthan: Ast.LessThan =>
          val lhs = lessthan.lhs.eval(world)
          val rhs = lessthan.rhs.eval(world)

          (lhs, rhs) match {
            case (l: Ast.Reference, r) => new Ast.LessThan(l.eval(world), r).eval(world)
            case (l, r: Ast.Reference) => new Ast.LessThan(l, r.eval(world)).eval(world)
            case (l: Ast.Integer, r: Ast.Integer) => Ast.Bool(l.value < r.value)
            case _ =>
              throw new UnsupportedOperationException

          }

        case morethan: Ast.MoreThan =>
          val lhs = morethan.lhs.eval(world)
          val rhs = morethan.rhs.eval(world)

          (lhs, rhs) match {
            case (l: Ast.Reference, r) => new Ast.MoreThan(l.eval(world), r).eval(world)
            case (l, r: Ast.Reference) => new Ast.MoreThan(l, r.eval(world)).eval(world)
            case (l: Ast.Integer, r: Ast.Integer) => Ast.Bool(l.value > r.value)
            case _ => throw new UnsupportedOperationException
          }

      }
    }
  }

}
