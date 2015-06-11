import scala.util.parsing.combinator._
import scala.xml._

class SimpleXMLParser extends RegexParsers {
  val tagName = """[a-zA-Z0-9]+""".r

  // into, accept

  val doubleQuotedString = """\"(.+?)\"""".r
  val singleQuotedString = """\'(.+?)\'""".r
  def quotedString : Parser[String] = doubleQuotedString | singleQuotedString ^^ {
    case doubleQuotedString(f) => f
    case singleQuotedString(f) => f
  }
  def attrValue : Parser[Tuple2[String,String]] = tagName ~ "=" ~ quotedString ^^ {
    case name ~ _ ~ value => (name, value)
  }

  def attrs : Parser[Map[String,String]] = rep(attrValue) ^^ {
    case r => r.toMap
  }

  def tagStart = tagName ~ attrs ^^ {
    case p ~ j => (p, j)
  }


  def singleTag : Parser[scala.xml.Elem] = "<" ~> tagStart <~ """/>""" ^^ {
    case t => {
      val seq = for( (n,v) <- t._2 ) yield new UnprefixedAttribute(n,v,Null)
      (new scala.xml.Elem(null, t._1, scala.xml.Null, scala.xml.TopScope, true) /: seq) ( _ % _ )
    }
  }

  def startTag = "<" ~> tagStart <~ ">"

  def nestedTag : Parser[scala.xml.Elem] = startTag >> { x  => success(x) ~ rep(expr) <~ "</" <~ x._1 <~ ">" } ^^ {
    case p ~ j => {
      val seq = for( (n,v) <- p._2 ) yield new UnprefixedAttribute(n,v,Null)
      (new scala.xml.Elem(null, p._1, scala.xml.Null, scala.xml.TopScope, true, j:_*) /: seq) ( _ % _ )

    }
  }

  def expr : Parser[scala.xml.Elem] = nestedTag | singleTag
}

val parser = new SimpleXMLParser
val result = parser.parseAll(parser.expr,"<test fun='1'><hi/><bar/><foo><car hi='asdf' asd='ddd'/></foo></test>")
//val result = parser.parseAll(parser.expr,"<test fun='1'></test>")
println(result)

