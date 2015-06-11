import scala.util.parsing.combinator._
import java.util.GregorianCalendar
import java.util.Date
class ListParser extends RegexParsers {
  val justDate =  """(\d{4})-(\d{2})-(\d{2})""".r
  val fullDateTime = """(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})(-|\+)(\d{2}):(\d{2})""".r


  def date: Parser[java.util.Date] = (fullDateTime | justDate) ^^ {
    case t => {
      t match {
        case justDate(year, month, day) => {
          val cal = new GregorianCalendar(year.toInt,
            month.toInt-1,
            day.toInt,
            0, 0, 0)
          cal.getTime()
        }
        case fullDateTime(year, month, day, hour, min, sec, _, _, _) => {
          val cal = new GregorianCalendar(year.toInt,
            month.toInt-1,
            day.toInt,
            hour.toInt,
            min.toInt,
            sec.toInt)
          cal.getTime()
        }
      }
    }
  }
}

val parser = new ListParser

val dates = Array("2015-06-03T13:21:58+00:00",
  "2012-08-20")

for(i <- dates) {
  val result = parser.parseAll(parser.date,i)
  println(result)
}
