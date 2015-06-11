import scala.util.parsing.combinator._

class ExprParser extends RegexParsers {
  val number = "[0-9]+".r

  def expr: Parser[Int] = term ~ opt(("+" | "-" | "%") ~ expr) ^^ {
    case t ~ None => t
    case t ~ Some("+" ~ e) => t + e
    case t ~ Some("-" ~ e) => t - e
    case t ~ Some("%" ~ e) => t % e
  }

  def term: Parser[Int] = exp ~ rep(("*" | "/") ~ exp) ^^ {
    case f ~ r  => r.foldLeft(f)((x, y) => {
      y._1 match {
        case "*" => x * y._2
        case "/" => x / y._2
      }
    })   
  }

  def exp: Parser[Int] = factor ~ rep("^" ~> factor) ^^ {
    case f ~ r => (f +: r).reduceRight((x, y) => Math.pow(x.toDouble, y.toDouble).toInt)
  }

  def factor: Parser[Int] = number ^^ { _.toInt } | "(" ~> expr <~ ")" 
}

val parser = new ExprParser

val result = parser.parseAll(parser.expr, "4^2^3")

println(result)
