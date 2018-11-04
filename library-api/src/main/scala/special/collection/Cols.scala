package special.collection

import scala.reflect.ClassTag
import scalan._

@ContainerType
@FunctorType
@scalan.Liftable
trait Col[A] {
  def builder: ColBuilder
  def arr: Array[A]
  def length: Int
  def apply(i: Int): A
  def getOrElse(i: Int, default: A): A
  def map[B: ClassTag](f: A => B): Col[B]
  def zip[B](ys: Col[B]): PairCol[A, B]
  def foreach(f: A => Unit): Unit
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean
  def filter(p: A => Boolean): Col[A]
  def where(p: A => Boolean): Col[A] = this.filter(p)
  def fold[B](zero: B, op: ((B, A)) => B): B
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

  @Internal
  private def trim[T](arr: Array[T]) = arr.take(arr.length min 100)
  @Internal
  override def toString = s"Col(${trim(arr).mkString(",")})"
}

trait PairCol[L,R] extends Col[(L,R)] {
  def ls: Col[L]
  def rs: Col[R]
}

trait ReplCol[A] extends Col[A] {
  def value: A
  def length: Int
}

@scalan.Liftable
trait ColBuilder {
  def pairCol[A,B](as: Col[A], bs: Col[B]): PairCol[A,B]

  def fromItems[T:ClassTag](items: T*): Col[T]

  @NeverInline
  def unzip[A,B](xs: Col[(A,B)]): (Col[A], Col[B]) = xs match {
    case pa: PairCol[_,_] => (pa.ls, pa.rs)
    case _ => ???
  }

  def xor(left: Col[Byte], right: Col[Byte]): Col[Byte]

  def fromArray[T](arr: Array[T]): Col[T]
  def replicate[T:ClassTag](n: Int, v: T): Col[T]
}

