package special.collection

class MonoidBuilderInst extends MonoidBuilder {
  val intPlusMonoid = new IntPlusMonoid(0)
  val longPlusMonoid = new LongPlusMonoid(0L)
}

class IntPlusMonoid(val zero: Int) extends Monoid[Int] {
  def plus(x: Int, y: Int): Int = x + y
  def power(x: Int, n: Int): Int = x * n
}

class LongPlusMonoid(val zero: Long) extends Monoid[Long] {
  def plus(x: Long, y: Long): Long = x + y
  def power(x: Long, n: Int): Long = x * n.toLong
}

