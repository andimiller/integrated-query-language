package net.andimiller.iql

object syntax {
  implicit class IQLSyntax(val sc: StringContext) extends AnyVal {
    def iql(args: Any*): Ast.Program = Parser.program.parse(sc.s(args:_*)).get.value
  }
  implicit class StringSyntax(s: String) {
    def unsafeIQLParse = Parser.program.parse(s).get.value
  }
  implicit class ProgramSyntax(p: Ast.Program) {
    def compile = Compiler.programCompiler.apply(p)
  }
}
