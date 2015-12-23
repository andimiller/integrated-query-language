
/**
  * Created by andi on 23/12/2015.
  */
object Evaluator {
  class TypeMappingFailed extends Exception
  trait Evaluator[A] {
    def eval(world: Ast.World): Ast.AnyVal
  }

  def anyToAst(a: Any): Option[Ast.AnyVal] = {
    a match {
      case s: String => Some(Ast.Text(s))
      case i: Integer => Some(Ast.Integer(i))
      case _ => throw new TypeMappingFailed
    }

  }

  implicit class EvaluatableReference(r: Ast.Reference) extends Evaluator[Ast.Reference] {
    override def eval(world: Ast.World): Ast.AnyVal = {
      r match {
        case f: Ast.Field =>
          world.globals.get(f.name).flatMap(anyToAst).getOrElse(Ast.None)
      }
    }
  }


  implicit class EvaluatablePipeline(p: Ast.Pipeline) extends Evaluator[Ast.Pipeline] {
    override def eval(world: Ast.World): Ast.AnyVal = {
      p match {
        case r: Ast.Reference => r.eval(world)
        case d: Ast.Data => d
      }
    }
  }


  implicit class EvaluatableExpression(e: Ast.Expression) extends Evaluator[Ast.Expression] {
    override def eval(world: Ast.World): Ast.AnyVal = {
      e match {
        case equals: Ast.Equals => Ast.Bool(equals.lhs.eval(world) == equals.rhs.eval(world))

      }
    }
  }

}
