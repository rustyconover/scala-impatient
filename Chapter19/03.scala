import scala.util.parsing.combinator._

class ListParser extends RegexParsers {
  val number = """-?\d+""".r
  def list: Parser[List[Int]] = repsep(expr, ",")
  def expr: Parser[Int] = number ^^ { _.toInt }
}

val parser = new ListParser
val result = parser.parseAll(parser.list, "1,23,-79,22,33")
println(result)
