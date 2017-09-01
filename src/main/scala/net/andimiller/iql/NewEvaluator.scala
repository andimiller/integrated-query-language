package net.andimiller.iql

import cats.data.{Reader, ReaderT, StateT}
import io.circe._
import cats.effect._, cats._, cats.implicits._

object NewEvaluator {

  case class State(input: Json, output: Json)

  type RunnableStep = ReaderT[IO, State, (State, Json)]
  trait Compiler[T] {
    def compile(t: T): RunnableStep
  }

  object referenceCompiler extends Compiler[Ast.Reference] {
    override def compile(t: Ast.Reference): RunnableStep = ReaderT { s: State =>
       t match {
         case Ast.Field(f) =>
           IO {
             (s, f.foldLeft(s.input.hcursor.asInstanceOf[ACursor]){ case (c, k) =>  c.downField(k) }.focus.getOrElse(Json.Null) )
           }
       }
    }
  }


  object pipelineCompiler extends Compiler[Ast.Pipeline] {
    override def compile(t: Ast.Pipeline): RunnableStep = ReaderT { s: State =>
      IO {
        t match {
          case r: Ast.Reference => referenceCompiler.compile(r).run(s).unsafeRunSync()
          case d: Ast.Data => d match {
            case t: Ast.Text => (s, Json.fromString(t.value))
            case i: Ast.Integer => (s, Json.fromInt(i.value))
            case f: Ast.Float => (s, Json.fromDoubleOrNull(f.value))
            case b: Ast.Bool => (s, Json.fromBoolean(b.value))
          }
          case e: Ast.InfixOperator => infixCompiler.compile(e).run(s).unsafeRunSync()
          case pe: Ast.PrefixOperator => prefixCompiler.compile(pe).run(s).unsafeRunSync()
        }
      }
    }
  }


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



  object infixCompiler extends Compiler[Ast.InfixOperator] {
    override def compile(t: Ast.InfixOperator): RunnableStep = ReaderT { s: State =>
      pipelineCompiler.compile(t.getLhs).run(s).flatMap { case (s1, lhs) =>
        pipelineCompiler.compile(t.getRhs).run(s).map { case (s2, rhs) =>
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
  }

  object prefixCompiler extends Compiler[Ast.PrefixOperator] {
    override def compile(t: Ast.PrefixOperator): RunnableStep = ReaderT { s: State =>
      pipelineCompiler.compile(t.getRhs).run(s).map { case (s1, rhs) =>
        val result = t match {
          case n: Ast.Not =>
            if (rhs.isBoolean) Json.fromBoolean(!rhs.asBoolean.get) else Json.Null
        }
        (s1, result)
      }
    }
  }

  object assignmentCompiler extends Compiler[Ast.Assignment] {
    override def compile(t: Ast.Assignment): RunnableStep = ReaderT { s: State =>
      pipelineCompiler.compile(t.rhs).run(s).map { case (s1, rhs) =>
        println(s"setting ${t.lhs.path} to $rhs")
        val output = t.lhs.path.foldLeft(s1.output.hcursor.asInstanceOf[ACursor]){_ path _}.withFocus{_ => rhs}.top.getOrElse(s1.output)
        (s1.copy(output = output), Json.Null)
      }
    }
  }


  object ProgramCompiler extends Compiler[Ast.Program] {
    override def compile(t: Ast.Program): RunnableStep = ReaderT { s0: State =>
      t.seq.map(assignmentCompiler.compile).foldLeft(IO.pure(s0)) { case (s, p) =>
        s.flatMap(p.run).map(_._1)
      }.map((_, Json.Null))
    }
  }

}