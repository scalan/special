package scalan.collection

import scala.reflect.ClassTag
import scalan.{OverloadId, SpecialPredef}

trait BaseColBuilder extends ColBuilder {
  @OverloadId("apply")       def apply[A, B](as: Col[A], bs: Col[B]): PairCol[A, B] = new PairOfCols(as, bs)
  @OverloadId("apply_items") def apply[T](items: T*): Col[T] = ???
  def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray[T](arr)
  def replicate[T:ClassTag](n: Int, v: T) = this.fromArray(Array.fill(n)(v))
  def dot[A](xs: Col[A], ys: Col[A]): A = ???
}

class ColOverArray[A](val arr: Array[A]) extends Col[A] {
  def builder = new ColOverArrayBuilder
  def length = arr.length
  def apply(i: Int) = arr(i)
  def map[B: ClassTag](f: A => B): Col[B] = builder.fromArray(arr.map(f))
  def foreach(f: A => Unit): Unit = arr.foreach(f)
  def exists(p: A => Boolean) = arr.exists(p)
  def forall(p: A => Boolean) = arr.forall(p)
  def filter(p: A => Boolean) = builder.fromArray(arr.filter(p))
  def fold[B](zero: B)(op: ((B, A)) => B) = arr.foldLeft(zero)((b, a) => op((b, a)))
  def slice(from: Int, until: Int) = builder.fromArray(arr.slice(from, until))
  def sum(m: Monoid[A]) = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  //  def ++(other: Col[A]) = builder.fromArray(arr ++ other.arr)
}

class ColOverArrayBuilder extends BaseColBuilder {
}

class PairOfCols[L,R](val ls: Col[L], val rs: Col[R]) extends PairCol[L,R] {
  override def builder: ColBuilder = new ColOverArrayBuilder
  override def arr: Array[(L, R)] = ls.arr.zip(rs.arr)
  override def length: Int = ls.length
  override def apply(i: Int): (L, R) = (ls(i), rs(i))
  override def map[V: ClassTag](f: ((L, R)) => V): Col[V] = new ColOverArray(arr.map(f))
  override def foreach(f: ((L, R)) => Unit): Unit = arr.foreach(f)
  override def exists(p: ((L, R)) => Boolean) = arr.exists(p)
  override def forall(p: ((L, R)) => Boolean) = arr.forall(p)
  override def filter(p: ((L, R)) => Boolean): Col[(L,R)] = new ColOverArray(arr.filter(p))
  override def fold[B](zero: B)(op: ((B, (L, R))) => B) = arr.foldLeft(zero)((b, a) => op((b,a)))
  override def slice(from: Int, until: Int) = builder(ls.slice(from, until), rs.slice(from, until))
  override def sum(m: Monoid[(L, R)]) = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  //  override def ++(other: Col[(L, R)]) =
}

class ReplCol[A](val value: A, val length: Int)(implicit cA: ClassTag[A]) extends Col[A] {
  def builder: ColBuilder = new ReplColBuilder
  def arr: Array[A] = builder.replicate(length, value).arr
  def apply(i: Int): A = value
  def map[B: ClassTag](f: A => B): Col[B] = new ReplCol(f(value), length)
  def foreach(f: A => Unit): Unit = ???
  def exists(p: A => Boolean): Boolean = p(value)
  def forall(p: A => Boolean): Boolean = p(value)
  def filter(p: A => Boolean): Col[A] = if (p(value)) this else new ReplCol(value, 0)
  def fold[B](zero: B)(op: ((B, A)) => B): B =
    SpecialPredef.loopUntil[(B, Int)]((zero,0),
      p => p._2 < length,
      p => (op((p._1, value)), p._2 + 1)
    )._1

  def slice(from: Int, until: Int): Col[A] = new ReplCol(value, until - from)
  def sum(m: Monoid[A]) = m.power(value, length)
}

class ReplColBuilder extends BaseColBuilder {
}

//  object ColOverArray {
//    def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray(arr)
//  }
//  class PairCol[A, B](val as: Col[A], val bs: Col[B]) extends Col[(A, B)] {
//    def arr: Array[(A, B)] = (as.arr zip bs.arr)
//    def length = as.length
//    def apply(i: Int) = (as(i), bs(i))
//  }

