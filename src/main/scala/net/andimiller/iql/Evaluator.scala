package net.andimiller.iql

import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

import scala.annotation.tailrec
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
      case n if n == null => Some(Ast.None)
      case i if i.isInt => Some(Ast.Integer(i.asInt()))
      case s if s.isTextual => Some(Ast.Text(s.asText()))
      case b if b.isBoolean => Some(Ast.Bool(b.asBoolean()))
      case a if a.isArray =>
        val array = a.elements().asScala.map(jsonNodeToAst).flatten.toSeq
        Some(Ast.Array(array))
    }
  }

  def putAstIntoJson(parent: ObjectNode, name: String, value: Ast.Data)(w: Ast.World) = {
    val primitiveValue = value match {
      case i: Ast.Integer =>
        parent.put(name, i.value)
      case t: Ast.Text    =>
        parent.put(name, t.value)
      case b: Ast.Bool    =>
        parent.put(name, b.value)
      case a: Ast.Array   => ???
      case n if n==Ast.None => ???
    }
  }

  implicit class EvaluatableReference(r: Ast.Reference) extends Evaluator[Ast.Reference] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      r match {
        case f: Ast.Field =>
          f.path.foldLeft(Seq(world.globals)) { (nodes, path) =>
            path match {
              // all-capturing star
              case s if s=="*" =>
                val arrays = nodes.filter(_.isArray)
                val maps = nodes.filter(_.isObject)
                arrays.flatMap(_.elements().asScala) ++ maps.flatMap(_.fields().asScala.map(_.getValue))
              // star being used to glob map keys
              case s if s.contains("*") =>
                val matcher = ("^"+s.replace("*", ".*")+"$").r.pattern
                nodes.flatMap(_.fields().asScala.filter(kv => matcher.matcher(kv.getKey).matches()).map(_.getValue))
              // array indexing
              case s if s.startsWith("[") && s.endsWith("]") && s.stripPrefix("[").stripSuffix("]").forall(_.isDigit) =>
                nodes.map(_.get(Integer.parseInt(s.stripPrefix("[").stripSuffix("]"))))
              // normal traversal
              case s =>
                nodes.flatMap(n => Option(n.get(s)))
            }
          }.flatMap(jsonNodeToAst) match {
            case empty    if empty.isEmpty   => Ast.None
            case item     if item.size==1    => item.head
            case multiple if multiple.size>1 => Ast.Array(multiple)
          }
        case f: Ast.OutputField =>
          // traverse the path, creating all the links we need, then return the parent and the name of the thing we're inserting
          val outputnode = f.path.init.foldLeft(world.output) { (node, path) =>
            path match {
              case s =>
                val target = Option(node.get(s))
                target match {
                  case Some(t) => t
                  case None =>
                    node.asInstanceOf[ObjectNode].putObject(path)
                }
            }
          }
          Ast.SettableOutputField(outputnode.asInstanceOf[ObjectNode], f.path.last)
        case sof: Ast.SettableOutputField =>
          throw new UnsupportedOperationException
      }
    }
  }


  implicit class EvaluatablePipeline(p: Ast.Pipeline) extends Evaluator[Ast.Pipeline] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      p match {
        case r: Ast.Reference => r.eval(world)
        case d: Ast.Data => d
        case e: Ast.InfixOperator => e.eval(world)
        case pe: Ast.PrefixOperator => pe.eval(world)
      }
    }
  }

  @tailrec
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

  implicit class EvaluatablePrefixExpression(pe: Ast.PrefixOperator) extends Evaluator[Ast.PrefixOperator] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      val rhs = resolveUntilData(pe.getRhs)(world)
      pe match {
        case n: Ast.Not =>
          rhs match {
            case rhs: Ast.Bool => Ast.Bool(!rhs.value)
            case _ =>
              throw new UnsupportedOperationException
          }
        case pe: Ast.PrefixOperator => throw new UnsupportedFunctionArgumentTypes
      }
    }
  }

  implicit class EvaluatableInfixExpression(e: Ast.InfixOperator) extends Evaluator[Ast.InfixOperator] {
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
        case and: Ast.AND =>
          (lhs, rhs) match {
            case (l: Ast.Bool, r: Ast.Bool) => Ast.Bool(l.value && r.value)
            case _ => throw new UnsupportedOperationException
          }
        case in: Ast.In =>
          (lhs, rhs) match {
            case (l: Ast.Data, r: Ast.Array) => Ast.Bool(r.values.contains(l))
            case _ => throw new UnsupportedOperationException
          }
        case xor: Ast.XOR =>
          (lhs, rhs) match {
            case (l: Ast.Bool, r: Ast.Bool) => Ast.Bool(l.value ^ r.value)
            case _ => throw new UnsupportedOperationException
          }
        case e: Ast.InfixOperator =>
          throw new UnsupportedOperationException
      }
    }
  }

  implicit class EvaluatableTransform(t: Ast.Transform) extends Evaluator[Ast.Transform] {
    override def eval(world: Ast.World): Ast.Pipeline = {
      t match {
        case assignment: Ast.Assignment =>
          val target = assignment.lhs.eval(world).asInstanceOf[Ast.SettableOutputField]
          val value = resolveUntilData(assignment.rhs)(world)
          putAstIntoJson(target.parent, target.field, value)(world)
          value
      }
    }
  }

  implicit class EvaluatableProgram(t: Ast.Program) {
    def eval(input: JsonNode): JsonNode = {
      val world = new Ast.World(input, OM.createObjectNode())
      t.seq.foreach(_.eval(world))
      world.output
    }
  }

}
