package scalan.util

object BenchmarkUtil {
  def measure[T](nIters: Int, okShowIterTime: Boolean = true)(action: Int => Unit): Unit = {
    var sum = 0L
    for (i <- 0 until nIters) {
      val start = System.currentTimeMillis()
      val res = action(i)
      val end = System.currentTimeMillis()
      val iterTime = end - start
      if (okShowIterTime)
        println(s"Iter $i: $iterTime ms")
      sum += iterTime
    }
    println(s"Total time: $sum ms")
  }
}
