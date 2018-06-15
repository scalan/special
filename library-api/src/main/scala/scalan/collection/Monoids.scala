package scalan.collection

trait Monoid[T] {
  def zero: T
  def plus(x: T, y: T): T
}

trait MonoidBuilder {
  val intPlusMonoid: Monoid[Int]
  val longPlusMonoid: Monoid[Long]
}


