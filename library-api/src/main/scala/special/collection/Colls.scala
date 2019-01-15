package special.collection

import scala.reflect.ClassTag
import scalan._

import scala.collection.GenSeq
import scala.collection.generic.CanBuildFrom

/**
  * @define Coll `Col`
  * @define coll collection
  * @tparam A the collection element type
  */
@ContainerType
@FunctorType
@scalan.Liftable
trait Coll[A] {
  def builder: ColBuilder
  def arr: Array[A]
  def length: Int
  def apply(i: Int): A
  def getOrElse(i: Int, default: A): A
  def map[B: ClassTag](f: A => B): Coll[B]

  /** For this collection (x0, ..., xN) and other collection (y0, ..., yM)
    * produces a collection ((x0, y0), ..., (xK, yK)) where K = min(N, M) */
  def zip[B](ys: Coll[B]): PairColl[A, B]

  def foreach(f: A => Unit): Unit
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean
  def filter(p: A => Boolean): Coll[A]
  def where(p: A => Boolean): Coll[A] = this.filter(p)
  def fold[B](zero: B, op: ((B, A)) => B): B

//  /** Produces the range of all indices of this collection [0 .. size-1] */
//  def indices: Col[Int]
//
//  /**
//    * Builds a new collection by applying a function to all elements of this $coll
//    * and using the elements of the resulting collections.
//    *
//    * @param f the function to apply to each element.
//    * @tparam B the element type of the returned collection.
//    * @return a new collection of type `Col[B]` resulting from applying the given collection-valued function
//    *         `f` to each element of this $coll and concatenating the results.
//    */
//  def flatMap[B: ClassTag](f: A => Col[B]): Col[B]

//  /** Computes length of longest segment whose elements all satisfy some predicate.
//    *
//    *  $mayNotTerminateInf
//    *
//    *  @param   p     the predicate used to test elements.
//    *  @param   from  the index where the search starts.
//    *  @return  the length of the longest segment of this $coll starting from index `from`
//    *           such that every element of the segment satisfies the predicate `p`.
//    */
//  def segmentLength(p: A => Boolean, from: Int): Int
//
//  /** Finds index of the first element satisfying some predicate after or at some start index.
//    *
//    *  $mayNotTerminateInf
//    *
//    *  @param   p     the predicate used to test elements.
//    *  @param   from   the start index
//    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
//    *           or `-1`, if none exists.
//    */
//  def indexWhere(p: A => Boolean, from: Int): Int
//
//  /** Finds index of last element satisfying some predicate before or at given end index.
//    *
//    *  @param   p     the predicate used to test elements.
//    *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
//    *           or `-1`, if none exists.
//    */
//  def lastIndexWhere(p: A => Boolean, end: Int): Int
//
//
//  /** Partitions this $coll in two ${coll}s according to a predicate.
//    *
//    *  @param pred the predicate on which to partition.
//    *  @return     a pair of ${coll}s: the first $coll consists of all elements that
//    *              satisfy the predicate `p` and the second $coll consists of all elements
//    *              that don't. The relative order of the elements in the resulting ${coll}s
//    *              will BE preserved (this is different from Scala's version of this method).
//    */
//  def partition(pred: A => Boolean): (Col[A], Col[A])
//
//  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
//    *
//    *  @param  from     the index of the first replaced element
//    *  @param  patch    the replacement sequence
//    *  @param  replaced the number of elements to drop in the original $coll
//    *  @return          a new $coll consisting of all elements of this $coll
//    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
//    */
//  def patch(from: Int, patch: Col[A], replaced: Int): Col[A]
//
//  /** A copy of this $coll with one single replaced element.
//    *  @param  index  the position of the replacement
//    *  @param  elem   the replacing element
//    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
//    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
//    */
//  def updated(index: Int, elem: A): Col[A]
//
//  /** Returns a copy of this collection where elements at `indexes` are replaced with `values`. */
//  def updateMany(indexes: Col[Int], values: Col[A]): Col[A]
//
//  /** Apply m for each element of this collection, group by key and reduce each group using r.
//    * @returns one item for each group in a new collection of (K,V) pairs. */
//  def mapReduce[K: ClassTag, V: ClassTag](m: A => (K,V), r: (V,V) => V): Col[(K,V)]
//
//  /** Produces a new collection which contains all distinct elements of this $coll and also all elements of
//    *  a given collection that are not in this collection.
//    *  This is order preserving operation considering only first occurrences of each distinct elements.
//    *  Any collection `xs` can be transformed to a sequence with distinct elements by using xs.unionSet(Col()).
//    *
//    *  NOTE: Use append if you don't need set semantics.
//    *
//    *  @param that   the collection to add.
//    */
//  def unionSets(that: Col[A]): Col[A]
//
//  /** Computes the multiset difference between this $coll and another sequence.
//    *
//    *  @param that   the sequence of elements to remove
//    *  @tparam B     the element type of the returned $coll.
//    *  @return       a new collection which contains all elements of this $coll
//    *                except some of occurrences of elements that also appear in `that`.
//    *                If an element value `x` appears
//    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
//    *                part of the result, but any following occurrences will.
//    */
//  def diff(that: Col[A]): Col[A]
//
//  /** Computes the multiset intersection between this $coll and another sequence.
//    *
//    *  @param that   the sequence of elements to intersect with.
//    *  @return       a new collection which contains all elements of this $coll
//    *                which also appear in `that`.
//    *                If an element value `x` appears
//    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
//    *                in the result, but any following occurrences will be omitted.
//    */
//  def intersect(that: Col[A]): Col[A]
//
  def sum(m: Monoid[A]): A

  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    */
  def slice(from: Int, until: Int): Coll[A]

  /** Puts the elements of other collection after the elements of this collection (concatenation of 2 collections) */
  def append(other: Coll[A]): Coll[A]

  @Internal
  private def trim[T](arr: Array[T]) = arr.take(arr.length min 100)
  @Internal
  override def toString = s"Col(${trim(arr).mkString(",")})"
}

trait PairColl[L,R] extends Coll[(L,R)] {
  def ls: Coll[L]
  def rs: Coll[R]
}

@Liftable
trait ReplColl[A] extends Coll[A] {
  def value: A
  def length: Int
  def append(other: Coll[A]): Coll[A]
}

@scalan.Liftable
trait ColBuilder {
  def pairCol[A,B](as: Coll[A], bs: Coll[B]): PairColl[A,B]

  @Reified("T") def fromItems[T](items: T*)(implicit cT: ClassTag[T]): Coll[T]

  @NeverInline
  def unzip[A,B](xs: Coll[(A,B)]): (Coll[A], Coll[B]) = xs match {
    case pa: PairColl[_,_] => (pa.ls, pa.rs)
    case _ => ???
  }

  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte]

  def fromArray[T](arr: Array[T]): Coll[T]
  def replicate[T:ClassTag](n: Int, v: T): Coll[T]
}

