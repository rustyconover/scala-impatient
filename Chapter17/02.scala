class Pair[T](var first : T, var second : T) {
  def swap() = {
    val third : T = first
    first = second
    second = third
  }
  override def toString = "Pair[First: " + first + " Second: " + second + "]"
}


val p = new Pair("String", List("1,2,3"))
println(p)
p.swap
println(p)

