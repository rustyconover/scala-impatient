import scala.xml.parsing._
import scala.xml._
import scala.xml.Attribute
import scala.io._

val p = new XhtmlParser(scala.io.Source.fromFile("example.xhtml")).initialize

val nodes = (p.document \\ "img").filter(_.attributes("alt") != null).map(_.asInstanceOf[Elem].copy() % Attribute(null, "alt", "TODO", Null))

println(nodes)
