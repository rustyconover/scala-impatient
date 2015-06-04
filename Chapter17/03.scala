class Pair[T,S] (val first : T, val second : S) {
  override def toString = "Pair[First: " + first + " Second: " + second + "]"
}

def swap[T,S](src : Pair[T,S]) : Pair[S,T] = new Pair(src.second, src.first)

val p = new Pair("String", List("1,2,3"))
println(p)
val s = swap(p)
println(s)
