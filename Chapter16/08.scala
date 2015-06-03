import scala.xml._

def parse(src : Node) = {
  src match {
    case <dl>{children @ _*}</dl> => {
      val types = children.groupBy(_.label)
      (types("dd") zip types("dt")).map(x => (x._1.text, x._2.text)).toMap
    }
  }
}

println(parse(<dl><dd>a</dd><dt>hello</dt><dd>b</dd><dt>there</dt></dl>))

