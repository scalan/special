package special.collection

import java.util

import special.SpecialPredef

import scala.reflect.ClassTag
import scalan.util.CollectionUtil
import scalan.util.CollectionUtil.unboxedArray
import scalan.{Internal, NeverInline, OverloadId}

class ColOverArray[A](val arr: Array[A]) extends Col[A] {
  def builder: ColBuilder = new ColOverArrayBuilder
  def length: Int = arr.length
  def apply(i: Int): A = arr(i)
  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < arr.length) arr(i) else default
  def map[B: ClassTag](f: A => B): Col[B] = builder.fromArray(arr.map(f))
  def foreach(f: A => Unit): Unit = arr.foreach(f)
  def exists(p: A => Boolean): Boolean = arr.exists(p)
  def forall(p: A => Boolean): Boolean = arr.forall(p)
  def filter(p: A => Boolean): Col[A] = builder.fromArray(arr.filter(p))
  @NeverInline
  def fold[B](zero: B, op: ((B, A)) => B): B = arr.foldLeft(zero)((b, a) => op((b, a)))
  def slice(from: Int, until: Int): Col[A] = builder.fromArray(arr.slice(from, until))
  def sum(m: Monoid[A]): A = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  def zip[B](ys: Col[B]): PairCol[A, B] = builder.pairCol(this, ys)
  @NeverInline
  def append(other: Col[A]): Col[A] = {
    if (arr.length <= 0) return other
    val result = CollectionUtil.concatArrays(arr, other.arr)
    builder.fromArray(result)
  }

  @Internal
  override def equals(obj: scala.Any) = obj match {
    case obj: Col[_] => util.Objects.deepEquals(obj.arr, arr)
    case _ => false
  }

  @Internal
  override def hashCode() = CollectionUtil.deepHashCode(arr)
}

class ColOverArrayBuilder extends ColBuilder {
  def pairCol[A, B](as: Col[A], bs: Col[B]): PairCol[A, B] = new PairOfCols(as, bs)

  @NeverInline
  def fromItems[T:ClassTag](items: T*): Col[T] = {
    new ColOverArray(unboxedArray(items))
  }

  def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray[T](arr)
  def replicate[T:ClassTag](n: Int, v: T): Col[T] = this.fromArray(Array.fill(n)(v))

  @NeverInline
  def xor(left: Col[Byte], right: Col[Byte]): Col[Byte] = fromArray(left.arr.zip(right.arr).map { case (l, r) => (l ^ r).toByte })
}

class PairOfCols[L,R](val ls: Col[L], val rs: Col[R]) extends PairCol[L,R] {
  override def builder: ColBuilder = new ColOverArrayBuilder
  override def arr: Array[(L, R)] = ls.arr.zip(rs.arr)
  override def length: Int = ls.length
  override def apply(i: Int): (L, R) = (ls(i), rs(i))
  @NeverInline
  override def getOrElse(i: Int, default: (L, R)): (L, R) =
    if (i >= 0 && i < this.length)
      this.apply(i)
    else {
      val d = default // force thunk
      (d._1, d._2)
    }
  override def map[V: ClassTag](f: ((L, R)) => V): Col[V] = new ColOverArray(arr.map(f))
  override def foreach(f: ((L, R)) => Unit): Unit = arr.foreach(f)
  override def exists(p: ((L, R)) => Boolean): Boolean = arr.exists(p)
  override def forall(p: ((L, R)) => Boolean): Boolean = arr.forall(p)
  override def filter(p: ((L, R)) => Boolean): Col[(L,R)] = new ColOverArray(arr.filter(p))
  @NeverInline
  override def fold[B](zero: B, op: ((B, (L, R))) => B): B = arr.foldLeft(zero)((b, a) => op((b,a)))
  override def slice(from: Int, until: Int): PairCol[L,R] = builder.pairCol(ls.slice(from, until), rs.slice(from, until))
  def append(other: Col[(L, R)]): Col[(L,R)] = {
    val arrs = builder.unzip(other)
    builder.pairCol(ls.append(arrs._1), rs.append(arrs._2))
  }
  @NeverInline
  override def sum(m: Monoid[(L, R)]): (L, R) = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  def zip[B](ys: Col[B]): PairCol[(L,R), B] = builder.pairCol(this, ys)
}

class CReplCol[A](val value: A, val length: Int)(implicit cA: ClassTag[A]) extends ReplCol[A] {
  def builder: ColBuilder = new ColOverArrayBuilder
  def arr: Array[A] = builder.replicate(length, value).arr
  def apply(i: Int): A = value
  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < this.length) value else default
  def map[B: ClassTag](f: A => B): Col[B] = new CReplCol(f(value), length)
  @NeverInline
  def foreach(f: A => Unit): Unit = (0 until length).foreach(_ => f(value))
  def exists(p: A => Boolean): Boolean = p(value)
  def forall(p: A => Boolean): Boolean = p(value)
  def filter(p: A => Boolean): Col[A] = if (p(value)) this else new CReplCol(value, 0)
  @NeverInline
  def fold[B](zero: B, op: ((B, A)) => B): B =
    SpecialPredef.loopUntil[(B, Int)]((zero,0),
      p => p._2 < length,
      p => (op((p._1, value)), p._2 + 1)
    )._1

  def zip[B](ys: Col[B]): PairCol[A, B] = builder.pairCol(this, ys)

  def slice(from: Int, until: Int): Col[A] = new CReplCol(value, until - from)

  @NeverInline
  def append(other: Col[A]): Col[A] = builder.fromArray(arr).append(builder.fromArray(other.arr))

  def sum(m: Monoid[A]): A = m.power(value, length)
}

