package net.andimiller.iql

object Parser {
  import fastparse.all._

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T)         = f(t)
    override def toString() = name
  }

  // basics
  val Whitespace  = NamedFunction(" \n".contains(_: Char), "Whitespace")
  val Digits      = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space     = P(CharsWhile(Whitespace).?)
  val digits    = P(CharsWhile(Digits))
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val letter    = P(lowercase | uppercase)

  val hexDigit      = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  // data structures
  val array =
    P("[" ~/ Expression ~ ("," ~ space.? ~ Expression).rep.? ~ "]").map(t => t._2.map(_.+:(t._1)).getOrElse(Seq(t._1))).map(Ast.Array)

  // types
  val strChars = P(CharsWhile(StringChars))
  val string =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Ast.Text)
  val wildcard = P("*")
  //val squarebrackets = P("[" | "]")
  val referenceChars = P(letter | digits | wildcard) //| squarebrackets)
  val reference =
    P(("." ~ referenceChars.rep.!) ~/ ("." ~/ referenceChars.rep.!).rep).map(t =>
      t match {
        case (head, tail) if (head == "") && tail.isEmpty => Ast.Field(Seq())
        case (head, tail)                                 => Ast.Field(tail.+:(head))
    })
  val outputReferenceChars = P(letter | digits)
  val outputReference =
    P(("." ~ outputReferenceChars.rep.!) ~/ ("." ~/ outputReferenceChars.rep.!).rep).map(t =>
      t match {
        case (head, tail) if (head == "") && tail.isEmpty => Ast.OutputField(Seq())
        case (head, tail)                                 => Ast.OutputField(tail.+:(head))
    })
  val number =
    P(digits ~ digits.rep).!.map(s => Ast.Integer(Integer.parseInt(s.toString)))
  val boolean = P("true" | "false").!.map(_ match {
    case "true"  => Ast.Bool(true)
    case "false" => Ast.Bool(false)
  })

  // Nots
  val Notted = P("!" ~ space.? ~/ Expression).map(Ast.Not)

  // code
  val Expression: Parser[Ast.Pipeline] = P(Notted | number | string | reference | boolean | array | bracketedExpression)
  val OperatorExpression =
    P(Expression ~ space.? ~ ("==" | "<" | ">" | "&&" | "||" | "^" | "in" | "+").! ~/ space.? ~/ Expression).map {
      case (l, operator, r) =>
        operator match {
          case "==" => Ast.Equals(l, r)
          case "<"  => Ast.LessThan(l, r)
          case ">"  => Ast.MoreThan(l, r)
          case "&&" => Ast.AND(l, r)
          case "||" => Ast.OR(l, r)
          case "^"  => Ast.XOR(l, r)
          case "in" => Ast.In(l, r)
          case "+"  => Ast.Plus(l, r)
        }
    }
  val bracketedExpression: Parser[Ast.InfixOperator] = P("(" ~/ OperatorExpression ~ ")")
  val toplevelExpression: Parser[Ast.Pipeline]       = P(P(Expression ~ newline) | P(OperatorExpression ~ newline))

  val function = P("required" | "int" | "bool" | "string").!

  // transforms
  val assignment = P(outputReference ~ space.? ~ "=" ~/ space.? ~ toplevelExpression).map(Ast.Assignment.tupled)

  // validation
  val validation = P(outputReference ~ space.? ~ ":" ~ space.? ~/ function).map(Ast.Validation.tupled)

  // full programs
  val newline           = P("\n" | "\r\n" | "\r" | "\f" | End)
  val program           = P(assignment.rep).map(t => Ast.Program(t))
  val validationProgram = P((validation ~/ newline).rep).map(t => Ast.VProgram(t))
}
