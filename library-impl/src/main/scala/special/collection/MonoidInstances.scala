package special.collection

class MonoidBuilderInst extends MonoidBuilder {
  val intPlusMonoid = new IntPlusMonoid(0)
  val longPlusMonoid = new LongPlusMonoid(0L)

  override def pairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)] =
    new PairMonoid(m1, m2)
}

class IntPlusMonoid(val zero: Int) extends Monoid[Int] {
  def plus(x: Int, y: Int): Int = x + y
  def power(x: Int, n: Int): Int = x * n
}

class LongPlusMonoid(val zero: Long) extends Monoid[Long] {
  def plus(x: Long, y: Long): Long = x + y
  def power(x: Long, n: Int): Long = x * n.toLong
}

class PairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](val m1: Monoid[A], m2: Monoid[B]) extends Monoid[(A,B)] {
  override def zero: (A, B) = (m1.zero, m2.zero)
  override def plus(x: (A, B), y: (A, B)): (A, B) = (m1.plus(x._1, y._1), m2.plus(x._2, y._2))
  override def power(x: (A, B), n: Int): (A, B) = (m1.power(x._1, n), m2.power(x._2, n))
}
