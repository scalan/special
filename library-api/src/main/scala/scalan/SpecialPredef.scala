package scalan

object SpecialPredef {
  def loopUntil[A](s1: A, isMatch: A => Boolean, step: A => A): A = {
    var res = s1
    while (!isMatch(res))
      res = step(res)
    res
  }
}
