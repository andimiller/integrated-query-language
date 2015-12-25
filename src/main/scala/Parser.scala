
object Parser {
  import fastparse.all._

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override def toString() = name
  }

  // basics
  val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  val ReferenceChars = NamedFunction('a' to 'Z' contains(_: Char), "ReferenceChars")

  val space         = P( CharsWhile(Whitespace).? )
  val digits        = P( CharsWhile(Digits))
  val lowercase  = P( CharIn('a' to 'z') )
  val uppercase  = P( CharIn('A' to 'Z') )
  val letter     = P( lowercase | uppercase )

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  // data structures
  val array = P( "[" ~/ Expression ~ ("," ~ space.? ~ Expression).rep.? ~ "]").map( t => t._2.map(_.+:(t._1)).getOrElse(Seq(t._1)))

  // programs
  val strChars = P( CharsWhile(StringChars) )
  val string =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Ast.Text)
  val reference =
    P( (letter | digits) ~ (letter | digits).rep).!.map(Ast.Field)
  val number =
    P(  digits ~ digits.rep).!.map(s => Ast.Integer(Integer.parseInt(s.toString)))
  val Expression: Parser[Ast.Pipeline] = P(number | string | reference | bracketedExpression)
  val Equality =
    P( Expression ~/ space.? ~/  "=" ~/ space.? ~/ Expression).map(t => new Ast.Equals(t._1, t._2))
  val LessThan =
    P( Expression ~/ space.? ~/  "<" ~/ space.? ~/ Expression).map(t => new Ast.LessThan(t._1, t._2))
  val MoreThan =
    P( Expression ~/ space.? ~/  ">" ~/ space.? ~/ Expression).map(t => new Ast.MoreThan(t._1, t._2))
  val AND =
    P( Expression ~/ space.? ~/  "&&" ~/ space.? ~/ Expression).map(t => new Ast.AND(t._1, t._2))
  val OR =
    P( Expression ~/ space.? ~/  "||" ~/ space.? ~/ Expression).map(t => new Ast.OR(t._1, t._2))
  val XOR =
    P( Expression ~/ space.? ~/  "^" ~/ space.? ~/ Expression).map(t => new Ast.XOR(t._1, t._2))
  val operatorExpression = P(Equality | LessThan | MoreThan | AND | OR | XOR)
  val bracketedExpression: Parser[Ast.Expression] = P("(" ~/ operatorExpression ~ ")")


}

