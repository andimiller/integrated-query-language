package net.andimiller.iql

import io.circe.Json

object syntax {
  implicit class IQLSyntax(val sc: StringContext) extends AnyVal {
    def iql(args: Any*): Ast.Program = Parser.program.parse(sc.s(args: _*)).get.value
  }
  implicit class StringSyntax(s: String) {
    def unsafeIQLParse  = Parser.program.parse(s).get.value
    def unsafeIQLParseV = Parser.validationProgram.parse(s).get.value
  }
  implicit class ProgramSyntax(p: Ast.Program) {
    def compile = Compiler.programCompiler.apply(p)
  }
  implicit class JsonSyntax(val j: Json) extends AnyVal {
    @inline def flatten: Json = utils.Flatten.flatten(j)
  }
}
