package special.collection

import java.util

import special.SpecialPredef

import scala.reflect.ClassTag
import scalan.util.CollectionUtil
import scalan.util.CollectionUtil.{unboxedArray}
import scalan.{Internal, NeverInline, OverloadId, Reified}
import Helpers._

class CollOverArray[A](val arr: Array[A])(implicit cA: ClassTag[A]) extends Coll[A] {
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

  @NeverInline
  override def segmentLength(p: A => Boolean, from: Int): Int = arr.segmentLength(p, from)

  @NeverInline
  override def indexWhere(p: A => Boolean, from: Int): Int = arr.indexWhere(p, from)

  @NeverInline
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = arr.lastIndexWhere(p, end)

  @NeverInline
  override def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
    val (ls, rs) = arr.partition(pred)
    (builder.fromArray(ls), builder.fromArray(rs))
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A] = {
    val res = arr.patch(from, patch.arr, replaced).toArray
    builder.fromArray(res)
  }

  @NeverInline
  override def updated(index: Int, elem: A): Coll[A] = {
    val res = arr.updated(index, elem)
    builder.fromArray(res)
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A] = {
    requireSameLength(indexes, values)
    val resArr = arr.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= arr.length) throw new IndexOutOfBoundsException(pos.toString)
      resArr(pos) = values(i)
      i += 1
    }
    builder.fromArray(resArr)
  }

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
  override def Monoids: MonoidBuilder = new MonoidBuilderInst

  def pairColl[A, B](as: Coll[A], bs: Coll[B]): PairColl[A, B] = new PairOfCols(as, bs)

  @NeverInline
  @Reified("T")
  def fromItems[T](items: T*)(implicit cT: ClassTag[T]): Coll[T] = {
    new CollOverArray(unboxedArray(items))
  }

  @NeverInline
  def fromArray[T](arr: Array[T]): Coll[T] = {
    implicit val cT = ClassTag[T](arr.getClass.getComponentType.asInstanceOf[Class[T]])
    new CollOverArray[T](arr)
  }

  @NeverInline
  def replicate[T:ClassTag](n: Int, v: T): Coll[T] = new CReplColl(v, n) //this.fromArray(Array.fill(n)(v))

  @NeverInline
  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte] = fromArray(left.arr.zip(right.arr).map { case (l, r) => (l ^ r).toByte })

  @NeverInline
  override def emptyColl[T](implicit cT: ClassTag[T]): Coll[T] = new CollOverArray[T](Array[T]())
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

  @NeverInline
  override def segmentLength(p: ((L, R)) => Boolean, from: Int): Int = {
    arr.segmentLength(p, from)
  }

  @NeverInline
  override def indexWhere(p: ((L, R)) => Boolean, from: Int): Int = arr.indexWhere(p, from)

  @NeverInline
  override def lastIndexWhere(p: ((L, R)) => Boolean, end: Int): Int = arr.lastIndexWhere(p, end)

  @NeverInline
  override def partition(pred: ((L, R)) => Boolean): (Coll[(L, R)], Coll[(L, R)]) = {
    val (ls, rs) = arr.partition(pred)
    (builder.fromArray(ls), builder.fromArray(rs))
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[(L, R)], replaced: Int): Coll[(L, R)] = {
    val (lsPatch, rsPatch) = builder.unzip(patch)
    val lp = ls.patch(from, lsPatch, replaced)
    val rp = rs.patch(from, rsPatch, replaced)
    builder.pairColl(lp, rp)
  }

  @NeverInline
  override def updated(index: Int, elem: (L, R)): Coll[(L, R)] = {
    val lu = ls.updated(index, elem._1)
    val ru = rs.updated(index, elem._2)
    builder.pairColl(lu, ru)
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[(L, R)]): Coll[(L, R)] = {
    requireSameLength(indexes, values)
    val resL = ls.arr.clone()
    val resR = rs.arr.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= length) throw new IndexOutOfBoundsException(pos.toString)
      resL(pos) = values(i)._1
      resR(pos) = values(i)._2
      i += 1
    }
    builder.pairColl(builder.fromArray(resL), builder.fromArray(resR))
  }
}

class CReplColl[A](val value: A, val length: Int)(implicit cA: ClassTag[A]) extends ReplColl[A] {
  def builder: CollBuilder = new CollOverArrayBuilder
  @Internal
  private var _arr: Array[A] = _

  @NeverInline
  def arr: Array[A] = {
    if (_arr == null)
      _arr = Array.fill(length)(value)
    _arr
  }

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
      p => p._2 >= length,
      p => (op((p._1, value)), p._2 + 1)
    )._1

  def zip[B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)

  def slice(from: Int, until: Int): Coll[A] = {
    val lo = math.max(from, 0)
    val hi = math.min(math.max(until, 0), length)
    val size = math.max(hi - lo, 0)
    new CReplColl(value, size)
  }

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

  @NeverInline
  override def segmentLength(p: A => Boolean, from: Int): Int = {
    if (from >= length) 0
    else
    if (p(value)) length - from
    else 0
  }

  @NeverInline
  override def indexWhere(p: A => Boolean, from: Int): Int = {
    if (from >= length) -1
    else
    if (p(value)) math.max(from, 0)
    else -1
  }

  @NeverInline
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = math.min(end, length - 1)
    if (i < 0) i
    else if (p(value)) i
    else -1
  }

  @NeverInline
  override def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
    if (pred(value)) (this, builder.emptyColl)
    else (builder.emptyColl, this)
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A] = {
    builder.fromArray(arr.patch(from, patch.arr, replaced))
  }

  @NeverInline
  override def updated(index: Int, elem: A): Coll[A] = {
    if (elem == value) this
    else {
      val res = arr.updated(index, elem)
      builder.fromArray(res)
    }
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A] = {
    requireSameLength(indexes, values)
    val resArr = arr.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= length) throw new IndexOutOfBoundsException(pos.toString)
      resArr(pos) = values(i)
      i += 1
    }
    builder.fromArray(resArr)
  }

  @Internal
  override def toString = s"ReplColl($value, $length)"
}

