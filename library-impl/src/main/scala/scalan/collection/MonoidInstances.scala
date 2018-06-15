package scalan.collection

class MonoidBuilderInst extends MonoidBuilder {
  val intPlusMonoid = new IntPlusMonoid(0)
  val longPlusMonoid = new LongPlusMonoid(0L)
}

class IntPlusMonoid(val zero: Int) extends Monoid[Int] {
  def plus(x: Int, y: Int) = x + y
}

class LongPlusMonoid(val zero: Long) extends Monoid[Long] {
  def plus(x: Long, y: Long) = x + y
}

