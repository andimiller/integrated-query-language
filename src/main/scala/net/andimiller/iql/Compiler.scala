package net.andimiller.iql

import cats.data.{Reader, ReaderT, StateT}
import io.circe._
import cats.effect._
import cats._
import cats.implicits._

object Compiler {

  implicit class PathCreatingCursor(cursor: ACursor) {
    def path(s: String): ACursor = {
      val attempt = cursor.downField(s)
      attempt.failed match {
        case true =>
          cursor.withFocus(_.mapObject(_.add(s, Json.obj()))).downField(s)
        case false => attempt
      }
    }
  }

  object CirceMatchers {
    object JNumber {
      def unapply(j: Json): Option[Double] =
        Option(j).flatMap(_.asNumber.map(_.toDouble))
    }
    object JInteger {
      def unapply(j: Json): Option[Int] =
        Option(j).flatMap(_.asNumber.flatMap(_.toInt))
    }
    object JBoolean {
      def unapply(j: Json): Option[Boolean] = Option(j).flatMap(_.asBoolean)
    }
    object JArray {
      def unapply(j: Json): Option[Vector[Json]] = Option(j).flatMap(_.asArray)
    }
  }
  import CirceMatchers._

  case class State(input: Json, output: Json)

  type RunnableStep = ReaderT[IO, State, (State, Json)]
  type Compiler[T]  = T => RunnableStep

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

  val pipelineCompiler: Compiler[Ast.Pipeline] = (t: Ast.Pipeline) =>
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
                .map(pipelineCompiler)
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
      pipelineCompiler(t.getLhs).run(s).flatMap {
        case (s1, lhs) =>
          pipelineCompiler(t.getRhs).run(s).map {
            case (s2, rhs) =>
              val result = t match {
                case equals: Ast.Equals => Json.fromBoolean(lhs == rhs)
                case lessthan: Ast.LessThan =>
                  (lhs, rhs) match {
                    case (JNumber(l), JNumber(r)) => Json.fromBoolean(l < r)
                  }
                case morethan: Ast.MoreThan =>
                  (lhs, rhs) match {
                    case (JNumber(l), JNumber(r)) => Json.fromBoolean(l > r)
                  }
                case or: Ast.OR =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l || r)
                  }
                case and: Ast.AND =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l && r)
                  }
                case in: Ast.In =>
                  (lhs, rhs) match {
                    case (l, JArray(r)) => Json.fromBoolean(r.contains(l))
                  }
                case xor: Ast.XOR =>
                  (lhs, rhs) match {
                    case (JBoolean(l), JBoolean(r)) => Json.fromBoolean(l ^ r)
                  }
                case plus: Ast.Plus =>
                  (lhs, rhs) match {
                    case (JInteger(l), JInteger(r)) => Json.fromInt(l + r)
                  }
              }
              (s2, result)
          }
      }
  }

  val prefixCompiler: Compiler[Ast.PrefixOperator] = (t: Ast.PrefixOperator) =>
    ReaderT { s: State =>
      pipelineCompiler(t.getRhs).run(s).map {
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
      pipelineCompiler(t.rhs).run(s).map {
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

  val programCompiler: Compiler[Ast.Program] = (t: Ast.Program) =>
    ReaderT { s0: State =>
      t.seq
        .map(assignmentCompiler)
        .foldLeft(IO.pure(s0)) {
          case (s, p) =>
            s.flatMap(p.run).map(_._1)
        }
        .map((_, Json.Null))
  }

  val vprogramCompiler: Compiler[Ast.VProgram] = (t: Ast.VProgram) =>
    ReaderT { s0: State =>
      t.seq
        .map(validationCompiler)
        .foldLeft(IO.pure(s0)) {
          case (s, p) =>
            s.flatMap(p.run).map(_._1)
        }
        .map((_, Json.Null))

  }
}
