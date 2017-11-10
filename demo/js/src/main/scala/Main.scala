import io.circe.Json
import net.andimiller.iql.syntax._
import net.andimiller.iql.Compiler.State
import io.circe.parser._, io.circe._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val program = ".foo = .bar".unsafeIQLParse.compile
    val json = parse("""{"bar": 42}""").right.get
    val result = program.run(State.forInput(json)).unsafeRunSync()
    println(result)
  }
}
