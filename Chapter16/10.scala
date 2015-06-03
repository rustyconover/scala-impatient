import scala.xml.parsing._
import scala.xml._
import scala.xml.transform._
import scala.xml.Attribute
import scala.xml.dtd._
import scala.io._

val p = new XhtmlParser(scala.io.Source.fromFile("example.xhtml")).initialize

val rule1 = new RewriteRule {
  override def transform(n: Node) = n match {
    case img @ <img/> if img.attributes("alt") != null => img.asInstanceOf[Elem].copy() % Attribute(null, "alt", "TODO", Null)
    case _ => n
  }
}

val transformed = new RuleTransformer(rule1).transform(p.document)

XML.save("fixed.xml", transformed(0),
  enc = "UTF-8",
  xmlDecl = true,
  doctype = DocType("html", PublicID("-//W3C//DTD XHTML 1.0 Strict//EN",
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"), Nil)
)

