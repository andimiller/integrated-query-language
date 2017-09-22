package net.andimiller.iql

import cats.data.{Reader, ReaderT, StateT}
import io.circe._
import cats.effect._
import cats._
import cats.implicits._

object Compiler {

  import utils.CirceHelpers._, CirceMatchers._

  case class State(input: Json, output: Json)
  object State {
    val empty = State(Json.obj(), Json.obj())
  }

  type RunnableStep = ReaderT[IO, State, (State, Json)]
  type Compiler[T]  = T => RunnableStep
  type Program = ReaderT[IO, State, State]
  type ProgramCompiler[T] = T => Program

  val referenceCompiler: Compiler[Ast.Reference] = (t: Ast.Reference) =>
    ReaderT { s: State =>
      t match {
        case Ast.Field(f) =>
          IO {
            (s,
             f.foldLeft(s.input.hcursor.asInstanceOf[ACursor]) {
                 case (c, k) => c.downField(k)
               }
               .focus
               .getOrElse(Json.Null))
          }
      }
  }

  val expressionCompiler: Compiler[Ast.Expression] = (t: Ast.Expression) =>
    ReaderT { s: State =>
      t match {
        case r: Ast.Reference => referenceCompiler(r).run(s)
        case d: Ast.Data =>
          d match {
            case t: Ast.Text    => IO { (s, Json.fromString(t.value)) }
            case i: Ast.Integer => IO { (s, Json.fromInt(i.value)) }
            case f: Ast.Float   => IO { (s, Json.fromDoubleOrNull(f.value)) }
            case b: Ast.Bool    => IO { (s, Json.fromBoolean(b.value)) }
            case a: Ast.Array =>
              a.values
                .map(expressionCompiler)
                .foldLeft(IO.pure(s, List.empty[Json])) {
                  case (i, r) =>
                    i.flatMap {
                      case (state, j) =>
                        r.run(state).map {
                          case (sr, jr) => (sr, j :+ jr)
                        }
                    }
                }
                .map {
                  case (finalstate, jsons) => (finalstate, Json.arr(jsons: _*))
                }
          }
        case e: Ast.InfixOperator   => infixCompiler(e).run(s)
        case pe: Ast.PrefixOperator => prefixCompiler(pe).run(s)
      }
  }

  val infixCompiler: Compiler[Ast.InfixOperator] = (t: Ast.InfixOperator) =>
    ReaderT { s: State =>
      expressionCompiler(t.getLhs).run(s).flatMap {
        case (s1, lhs) =>
          expressionCompiler(t.getRhs).run(s).map {
            case (s2, rhs) =>
              val result = t match {
                case equals: Ast.Equals => Json.fromBoolean(lhs == rhs)
                case lessthan: Ast.LessThan =>
                  (lhs, rhs) match {
                    case (JNumber(l), JNumber(r)) => Json.fromBoolean(l < r)
                    case _                        => Json.Null
                  }
                case morethan: Ast.MoreThan =>
                  (lhs, rhs) match {
                    case (JNumber(l), JNumber(r)) => Json.fromBoolean(l > r)
                    case _                        => Json.Null
                  }
                case or: Ast.OR =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l || r)
                    case _                          => Json.Null
                  }
                case and: Ast.AND =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l && r)
                    case _                          => Json.Null
                  }
                case in: Ast.In =>
                  (lhs, rhs) match {
                    case (l, JArray(r)) => Json.fromBoolean(r.contains(l))
                    case _              => Json.Null
                  }
                case xor: Ast.XOR =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l ^ r)
                    case _                          => Json.Null
                  }
                case plus: Ast.Plus =>
                  (lhs, rhs) match {
                    case (JInteger(l), JInteger(r)) => Json.fromInt(l + r)
                    case (JFloat(l), JInteger(r)) =>
                      Json.fromDouble(l + r).getOrElse(Json.Null)
                    case (JInteger(l), JFloat(r)) =>
                      Json.fromDouble(l + r).getOrElse(Json.Null)
                    case (JFloat(l), JFloat(r)) =>
                      Json.fromDouble(l + r).getOrElse(Json.Null)
                    case (JString(l), JString(r)) => Json.fromString(l + r)
                    case _                        => Json.Null
                  }
              }
              (s2, result)
          }
      }
  }

  val prefixCompiler: Compiler[Ast.PrefixOperator] = (t: Ast.PrefixOperator) =>
    ReaderT { s: State =>
      expressionCompiler(t.getRhs).run(s).map {
        case (s1, rhs) =>
          val result = t match {
            case n: Ast.Not =>
              if (rhs.isBoolean) Json.fromBoolean(!rhs.asBoolean.get)
              else Json.Null
          }
          (s1, result)
      }
  }

  val assignmentCompiler: Compiler[Ast.Assignment] = (t: Ast.Assignment) =>
    ReaderT { s: State =>
      expressionCompiler(t.rhs).run(s).map {
        case (s1, rhs) =>
          val output = t.lhs.path
            .foldLeft(s1.output.hcursor.asInstanceOf[ACursor]) { _ path _ }
            .withFocus { _ =>
              rhs
            }
            .top
            .getOrElse(s1.output)
          (s1.copy(output = output), Json.Null)
      }
  }

  val validationCompiler: Compiler[Ast.Validation] = (t: Ast.Validation) =>
    ReaderT { s: State =>
      IO {
        val item = t.lhs.path
          .foldLeft(s.input.hcursor.asInstanceOf[ACursor]) {
            case (c, k) => c.downField(k)
          }
          .focus
          .getOrElse(Json.Null)
        val result = t.rhs match {
          case "required" => !item.isNull
          case "int"      => item.isNumber
          case "string"   => item.isString
          case "boolean"  => item.isBoolean
        }
        val output = t.lhs.path
          .foldLeft(s.output.hcursor.asInstanceOf[ACursor]) { _ path _ }
          .withFocus(_ => Json.fromBoolean(result))
          .top
          .getOrElse(s.output)
        (s.copy(output = output), Json.Null)
      }
  }

  val programCompiler: ProgramCompiler[Ast.Program] = (t: Ast.Program) =>
    ReaderT { s0: State =>
      t.seq
        .map(assignmentCompiler)
        .foldLeft(IO.pure(s0)) {
          case (s, p) =>
            s.flatMap(p.run).map(_._1)
        }
  }

  val vprogramCompiler: ProgramCompiler[Ast.VProgram] = (t: Ast.VProgram) =>
    ReaderT { s0: State =>
      t.seq
        .map(validationCompiler)
        .foldLeft(IO.pure(s0)) {
          case (s, p) =>
            s.flatMap(p.run).map(_._1)
        }

  }
}
