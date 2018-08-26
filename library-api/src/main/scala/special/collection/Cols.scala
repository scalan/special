package special.collection

import scala.reflect.ClassTag
import scalan.{FunctorType, ContainerType, OverloadId, NeverInline}

@ContainerType
@FunctorType
trait Col[A] {
  def builder: ColBuilder
  def arr: Array[A]
  def length: Int
  def apply(i: Int): A
  def getOrElse(i: Int, default: => A): A
  def map[B: ClassTag](f: A => B): Col[B]
  def zip[B](ys: Col[B]): PairCol[A, B] = builder(this, ys)
  def foreach(f: A => Unit): Unit
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean
  def filter(p: A => Boolean): Col[A]
  def where(p: A => Boolean): Col[A] = this.filter(p)
  def fold[B](zero: B)(op: ((B, A)) => B): B
  def sum(m: Monoid[A]): A
  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    */
  def slice(from: Int, until: Int): Col[A]
  def append(other: Col[A]): Col[A]
}

trait PairCol[L,R] extends Col[(L,R)] {
  def ls: Col[L]
  def rs: Col[R]
}

trait ColBuilder {
  @OverloadId("apply")
  def apply[A,B](as: Col[A], bs: Col[B]): PairCol[A,B]

  @OverloadId("apply_items")
  def apply[T](items: T*): Col[T]

  @NeverInline
  def unzip[A,B](xs: Col[(A,B)]): (Col[A], Col[B]) = xs match {
    case pa: PairCol[_,_] => (pa.ls, pa.rs)
    case _ => ???
  }

  def xor(left: Col[Byte], right: Col[Byte]): Col[Byte]

  def fromItemsTest: Col[Int] = this.apply(1, 2, 3)
  def fromArray[T](arr: Array[T]): Col[T]
  def replicate[T:ClassTag](n: Int, v: T): Col[T]
  def dot[T](xs: Col[T], ys: Col[T]): T
  @throws[NullPointerException]
  def ddmvm(v: Array[Double]): Int = {
    val xs = Array.fill(v.length)(0)
    val c = xs.zip(v).map(d => d)
    c.length
  }
}

trait Enum {
  def value: Int
}
