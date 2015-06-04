class Pair[T,S](val first: T, val second: S) {
  def swap() : Pair[S,T] = new Pair(second, first)
  override def toString = "Pair[First: " + first + " Second: " + second + "]"
}


val p = new Pair("String", List("1,2,3"))
println(p)
val s = p.swap
println(s)
