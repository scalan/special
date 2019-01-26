package special.collection

trait Monoid[@specialized(Int, Long) T] {
  def zero: T
  def plus(x: T, y: T): T
  def power(x: T, n: Int): T
}

trait MonoidBuilder {
  def intPlusMonoid: Monoid[Int]
  def longPlusMonoid: Monoid[Long]
  def pairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)]
}


