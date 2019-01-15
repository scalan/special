package special.collection

import java.util

import special.SpecialPredef

import scala.reflect.ClassTag
import scalan.util.CollectionUtil
import scalan.util.CollectionUtil.unboxedArray
import scalan.{Internal, NeverInline, OverloadId, Reified}

class CollOverArray[A](val arr: Array[A]) extends Coll[A] {
  def builder: CollBuilder = new CollOverArrayBuilder
  def length: Int = arr.length
  def apply(i: Int): A = arr.apply(i)
  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < arr.length) arr(i) else default
  def map[B: ClassTag](f: A => B): Coll[B] = builder.fromArray(arr.map(f))
  def foreach(f: A => Unit): Unit = arr.foreach(f)
  def exists(p: A => Boolean): Boolean = arr.exists(p)
  def forall(p: A => Boolean): Boolean = arr.forall(p)
  def filter(p: A => Boolean): Coll[A] = builder.fromArray(arr.filter(p))
  @NeverInline
  def fold[B](zero: B, op: ((B, A)) => B): B = arr.foldLeft(zero)((b, a) => op((b, a)))
  def slice(from: Int, until: Int): Coll[A] = builder.fromArray(arr.slice(from, until))
  def sum(m: Monoid[A]): A = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  def zip[B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)
  @NeverInline
  def append(other: Coll[A]): Coll[A] = {
    if (arr.length <= 0) return other
    val result = CollectionUtil.concatArrays(arr, other.arr)
    builder.fromArray(result)
  }

  @NeverInline
  def indices: Coll[Int] = builder.fromArray(arr.indices.toArray)

  @NeverInline
  override def flatMap[B: ClassTag](f: A => Coll[B]): Coll[B] = builder.fromArray(arr.flatMap(x => f(x).arr))

//  @NeverInline
//  override def segmentLength(p: A => Boolean, from: Int): Int = arr.segmentLength(p, from)
//
//  @NeverInline
//  override def indexWhere(p: A => Boolean, from: Int): Int = arr.indexWhere(p, from)
//
//  @NeverInline
//  override def lastIndexWhere(p: A => Boolean, end: Int): Int = arr.lastIndexWhere(p, end)
//
//  @NeverInline
//  override def partition(pred: A => Boolean): (Coll[A], Coll[A]) = ???
//
//  override def patch(from: Int,
//      patch: Coll[A],
//      replaced: Int): Coll[A] = ???
//
//  override def updated(index: Int, elem: A): Coll[A] = ???
//
//  override def updateMany(indexes: Coll[Int],
//      values: Coll[A]): Coll[A] = ???
//
//  override def mapReduce[K: ClassTag, V: ClassTag](m: A => (K, V),
//      r: (V, V) => V): Coll[(K, V)] = ???
//
//  override def unionSets(that: Coll[A]): Coll[A] = ???
//
//  override def diff(that: Coll[A]): Coll[A] = ???
//
//  override def intersect(that: Coll[A]): Coll[A] = ???

  @Internal
  override def equals(obj: scala.Any) = obj match {
    case obj: Coll[_] => util.Objects.deepEquals(obj.arr, arr)
    case _ => false
  }

  @Internal
  override def hashCode() = CollectionUtil.deepHashCode(arr)
}

class CollOverArrayBuilder extends CollBuilder {
  def pairColl[A, B](as: Coll[A], bs: Coll[B]): PairColl[A, B] = new PairOfCols(as, bs)

  @NeverInline
  @Reified("T")
  def fromItems[T](items: T*)(implicit cT: ClassTag[T]): Coll[T] = {
    new CollOverArray(unboxedArray(items))
  }

  @NeverInline
  def fromArray[T](arr: Array[T]): Coll[T] = new CollOverArray[T](arr)

  @NeverInline
  def replicate[T:ClassTag](n: Int, v: T): Coll[T] = new CReplColl(v, n) //this.fromArray(Array.fill(n)(v))

  @NeverInline
  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte] = fromArray(left.arr.zip(right.arr).map { case (l, r) => (l ^ r).toByte })
}

class PairOfCols[L,R](val ls: Coll[L], val rs: Coll[R]) extends PairColl[L,R] {
  override def builder: CollBuilder = new CollOverArrayBuilder
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
  override def map[V: ClassTag](f: ((L, R)) => V): Coll[V] = new CollOverArray(arr.map(f))
  override def foreach(f: ((L, R)) => Unit): Unit = arr.foreach(f)
  override def exists(p: ((L, R)) => Boolean): Boolean = arr.exists(p)
  override def forall(p: ((L, R)) => Boolean): Boolean = arr.forall(p)
  override def filter(p: ((L, R)) => Boolean): Coll[(L,R)] = new CollOverArray(arr.filter(p))
  @NeverInline
  override def fold[B](zero: B, op: ((B, (L, R))) => B): B = arr.foldLeft(zero)((b, a) => op((b,a)))
  override def slice(from: Int, until: Int): PairColl[L,R] = builder.pairColl(ls.slice(from, until), rs.slice(from, until))
  def append(other: Coll[(L, R)]): Coll[(L,R)] = {
    val arrs = builder.unzip(other)
    builder.pairColl(ls.append(arrs._1), rs.append(arrs._2))
  }
  @NeverInline
  override def sum(m: Monoid[(L, R)]): (L, R) = arr.foldLeft(m.zero)((b, a) => m.plus(b, a))
  def zip[B](ys: Coll[B]): PairColl[(L,R), B] = builder.pairColl(this, ys)

  override def indices: Coll[Int] = ls.indices

  @NeverInline
  override def flatMap[B: ClassTag](f: ((L, R)) => Coll[B]): Coll[B] =
    builder.fromArray(arr.flatMap(p => f(p).arr))
}

class CReplColl[A](val value: A, val length: Int)(implicit cA: ClassTag[A]) extends ReplColl[A] {
  def builder: CollBuilder = new CollOverArrayBuilder
  def arr: Array[A] = Array.fill(length)(value)
  def apply(i: Int): A = value
  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < this.length) value else default
  def map[B: ClassTag](f: A => B): Coll[B] = new CReplColl(f(value), length)
  @NeverInline
  def foreach(f: A => Unit): Unit = (0 until length).foreach(_ => f(value))
  def exists(p: A => Boolean): Boolean = p(value)
  def forall(p: A => Boolean): Boolean = p(value)
  def filter(p: A => Boolean): Coll[A] = if (p(value)) this else new CReplColl(value, 0)
  @NeverInline
  def fold[B](zero: B, op: ((B, A)) => B): B =
    SpecialPredef.loopUntil[(B, Int)]((zero,0),
      p => p._2 < length,
      p => (op((p._1, value)), p._2 + 1)
    )._1

  def zip[B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)

  def slice(from: Int, until: Int): Coll[A] = new CReplColl(value, until - from)

  @NeverInline
  def append(other: Coll[A]): Coll[A] = builder.fromArray(arr).append(builder.fromArray(other.arr))

  def sum(m: Monoid[A]): A = m.power(value, length)

  @NeverInline
  override def indices: Coll[Int] = builder.fromArray((0 until length).toArray)

  @NeverInline
  override def flatMap[B: ClassTag](f: A => Coll[B]): Coll[B] = {
    val seg = f(value).arr
    val xs = Range(0, length).flatMap(_ => seg).toArray
    builder.fromArray(xs)
  }
}

