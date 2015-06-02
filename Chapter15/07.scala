import scala.annotation._

class SimpleCalculator {
  @tailrec def sum(src : Array[Int]) : Int = if (src.size == 0) 0 else src.head + sum(src.tail)
}

class DoubleTotal extends SimpleCalculator {
  override def sum(src : Array[Int]) = if (src.size == 0) 0 else src.head*2 + sum(src.tail)
}

var c = new SimpleCalculator()

var l = Array(3,2,4)

println(c.sum(l))
