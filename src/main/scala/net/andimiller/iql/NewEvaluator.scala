package net.andimiller.iql

import java.util.concurrent.RunnableScheduledFuture

import cats.data.{Reader, ReaderT, StateT}
import io.circe._
import cats.effect._
import cats._
import cats.implicits._

object NewEvaluator {

  implicit class PathCreatingCursor(cursor: ACursor) {
    def path(s: String): ACursor = {
      val attempt = cursor.downField(s)
      attempt.failed match {
        case true =>
          cursor.withFocus(_.mapObject(_.add(s, Json.obj()))).downField(s)
        case false =>  attempt
      }
    }
  }

  case class State(input: Json, output: Json)

  type RunnableStep = ReaderT[IO, State, (State, Json)]
  type Compiler[T] = T => RunnableStep

  val referenceCompiler: Compiler[Ast.Reference] = (t: Ast.Reference) =>
    ReaderT { s: State =>
      t match {
        case Ast.Field(f) =>
          IO {
            (s, f.foldLeft(s.input.hcursor.asInstanceOf[ACursor]){ case (c, k) => c.downField(k) }.focus.getOrElse(Json.Null) )
          }
      }
    }

  val pipelineCompiler: Compiler[Ast.Pipeline] = (t: Ast.Pipeline) =>
    ReaderT { s: State =>
      t match {
        case r: Ast.Reference => referenceCompiler(r).run(s)
        case d: Ast.Data => d match {
          case t: Ast.Text => IO { (s, Json.fromString(t.value)) }
          case i: Ast.Integer => IO { (s, Json.fromInt(i.value)) }
          case f: Ast.Float => IO { (s, Json.fromDoubleOrNull(f.value)) }
          case b: Ast.Bool => IO { (s, Json.fromBoolean(b.value)) }
        }
        case e: Ast.InfixOperator => infixCompiler(e).run(s)
        case pe: Ast.PrefixOperator => prefixCompiler(pe).run(s)
      }
    }


  val infixCompiler: Compiler[Ast.InfixOperator] = (t: Ast.InfixOperator) =>
     ReaderT { s: State =>
      pipelineCompiler(t.getLhs).run(s).flatMap { case (s1, lhs) =>
        pipelineCompiler(t.getRhs).run(s).map { case (s2, rhs) =>
          val result = t match {
            case equals: Ast.Equals => Json.fromBoolean(lhs == rhs)
            case lessthan: Ast.LessThan =>
              (lhs, rhs) match {
                case (l, r) if l.isNumber && r.isNumber => Json.fromBoolean(l.asNumber.get.toDouble < r.asNumber.get.toDouble)
              }
            case morethan: Ast.MoreThan =>
              (lhs, rhs) match {
                case (l, r) if l.isNumber && r.isNumber => Json.fromBoolean(l.asNumber.get.toDouble > r.asNumber.get.toDouble)
              }
            case or: Ast.OR =>
              (lhs, rhs) match {
                case (l, r) if l.isBoolean && r.isBoolean => Json.fromBoolean(l.asBoolean.get || r.asBoolean.get)
              }
            case and: Ast.AND =>
              (lhs, rhs) match {
                case (l, r) if l.isBoolean && r.isBoolean => Json.fromBoolean(l.asBoolean.get && r.asBoolean.get)
              }
            case in: Ast.In =>
              (lhs, rhs) match {
                case (l, r) if r.isArray => Json.fromBoolean(r.asArray.get.contains(l))
              }
            case xor: Ast.XOR =>
              (lhs, rhs) match {
                case (l, r) if l.isBoolean && r.isBoolean => Json.fromBoolean(l.asBoolean.get ^ r.asBoolean.get)
              }
          }
          (s2, result)
        }
      }
    }


  val prefixCompiler: Compiler[Ast.PrefixOperator] = (t: Ast.PrefixOperator) =>
    ReaderT { s: State =>
      pipelineCompiler(t.getRhs).run(s).map { case (s1, rhs) =>
        val result = t match {
          case n: Ast.Not =>
            if (rhs.isBoolean) Json.fromBoolean(!rhs.asBoolean.get) else Json.Null
        }
        (s1, result)
      }
    }

  val assignmentCompiler: Compiler[Ast.Assignment] = (t: Ast.Assignment) =>
    ReaderT { s: State =>
      pipelineCompiler(t.rhs).run(s).map { case (s1, rhs) =>
        val output = t.lhs.path.foldLeft(s1.output.hcursor.asInstanceOf[ACursor]){_ path _}.withFocus{_ => rhs}.top.getOrElse(s1.output)
        (s1.copy(output = output), Json.Null)
      }
    }

  val programCompiler: Compiler[Ast.Program] = (t: Ast.Program) =>
    ReaderT { s0: State =>
      t.seq.map(assignmentCompiler).foldLeft(IO.pure(s0)) { case (s, p) =>
        s.flatMap(p.run).map(_._1)
      }.map((_, Json.Null))
    }

}