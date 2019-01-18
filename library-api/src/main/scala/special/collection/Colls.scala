package special.collection

import scala.reflect.ClassTag
import scalan._

import scala.collection.{GenSeq, immutable}

/**
  * @define Coll `Coll`
  * @define coll collection
  * @tparam A the collection element type
  */
@ContainerType
@FunctorType
@scalan.Liftable
trait Coll[A] {
  def builder: CollBuilder
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

  /** Produces the range of all indices of this collection as a new collection
    * containing [0 .. length-1] values. */
  def indices: Coll[Int]

  /**
    * Builds a new collection by applying a function to all elements of this $coll
    * and using the elements of the resulting collections.
    *
    * Function `f` is constrained to be of the form `x => x.someProperty`, otherwise
    * it is illegal.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Coll[B]` resulting from applying the given collection-valued function
    *         `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B: ClassTag](f: A => Coll[B]): Coll[B]

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from  the index where the search starts.
    *  @return  the length of the longest segment of this $coll starting from index `from`
    *           such that every element of the segment satisfies the predicate `p`.
    */
  def segmentLength(p: A => Boolean, from: Int): Int

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  @NeverInline
  def find(p: A => Boolean): Option[A] = {
    val i = segmentLength(!p(_), 0)
    if (i < length) Some(this(i)) else None
  }


  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
    *
    *  @param   elem   the element value to search for.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    */
  @NeverInline
  def indexOf(elem: A, from: Int): Int = this.indexWhere(x => elem == x, from)

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    */
  def lastIndexWhere(p: A => Boolean, end: Int): Int


  /** Partitions this $coll in two ${coll}s according to a predicate.
    *
    *  @param pred the predicate on which to partition.
    *  @return     a pair of ${coll}s: the first $coll consists of all elements that
    *              satisfy the predicate `p` and the second $coll consists of all elements
    *              that don't. The relative order of the elements in the resulting ${coll}s
    *              will BE preserved (this is different from Scala's version of this method).
    */
  def partition(pred: A => Boolean): (Coll[A], Coll[A])

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    *  @param  from     the index of the first replaced element
    *  @param  patch    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
    */
  def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A]

  /** A copy of this $coll with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    */
  def updated(index: Int, elem: A): Coll[A]

  /** Returns a copy of this collection where elements at `indexes` are replaced with `values`. */
  def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A]

  /** Apply m for each element of this collection, group by key and reduce each group using r.
    * @returns one item for each group in a new collection of (K,V) pairs. */
  def mapReduce[K: ClassTag, V: ClassTag](m: A => (K,V), r: ((V,V)) => V): Coll[(K,V)]

  /** Produces a new collection which contains all distinct elements of this $coll and also all elements of
    *  a given collection that are not in this collection.
    *  This is order preserving operation considering only first occurrences of each distinct elements.
    *  Any collection `xs` can be transformed to a sequence with distinct elements by using xs.unionSet(Col()).
    *
    *  NOTE: Use append if you don't need set semantics.
    *
    *  @param that   the collection to add.
    */
  def unionSet(that: Coll[A]): Coll[A]

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
//  def diff(that: Coll[A]): Coll[A]
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
//  def intersect(that: Coll[A]): Coll[A]
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
  override def toString = s"Coll(${trim(arr).mkString(",")})"

  @Internal
  def cItem: ClassTag[A]

  @Internal
  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    var b = immutable.Map.empty[T,U]
    var i = 0
    val len = length
    while (i < len) {
      val kv = this(i)
      if (b.contains(kv._1))
        throw new IllegalArgumentException(s"Cannot transform collection $this to Map: duplicate key in entry $kv")
      b = b + kv
      i += 1
    }
    b
  }

  @Internal
  def distinctByKey[T, U](implicit ev: A <:< (T, U)): Coll[A] = {
    unionSetByKey(builder.emptyColl[A](cItem))
  }


  @Internal
  def unionSetByKey[T, U](that: Coll[A])(implicit ev: A <:< (T, U)): Coll[A] = {
    import scalan.util.CollectionUtil._
    // TODO optimize representation-wise
    val res = append(that).arr.toIterable.distinctBy(_._1)
//    val (ls, rs) = res.unzip
//    builder.pairCollFromArrays(ls.toArray, rs.toArray)
    builder.fromArray(res.toArray(cItem))
  }

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
trait CollBuilder {
  def Monoids: MonoidBuilder
  def pairColl[A,B](as: Coll[A], bs: Coll[B]): PairColl[A,B]

  @Internal
  def pairCollFromArrays[A,B](as: Array[A], bs: Array[B]): PairColl[A,B] =
    pairColl(fromArray(as), fromArray(bs))

  @Internal
  def fromMap[K:ClassTag,V:ClassTag](m: Map[K,V]): Coll[(K,V)]

  @Reified("T") def fromItems[T](items: T*)(implicit cT: ClassTag[T]): Coll[T]

  @NeverInline
  def unzip[A,B](xs: Coll[(A,B)]): (Coll[A], Coll[B]) = xs match {
    case pa: PairColl[_,_] => (pa.ls, pa.rs)
    case _ => ???
  }

  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte]

  def fromArray[T](arr: Array[T]): Coll[T]
  def replicate[T:ClassTag](n: Int, v: T): Coll[T]
  def emptyColl[T](implicit cT: ClassTag[T]): Coll[T]

  /** Performs outer join operation between left and right collections.
    * This is a restricted version of relational join operation.
    * It expects `left` and `right` collections have distinct K values in pairs (otherwise exception is thrown).
    * Under this condition resulting collection has size <= left.size + right.size.
    * @param l projection function executed for each element of `left`
    * @param r projection function executed for each element of `right`
    * @param inner projection function which is executed for matching items (K, L) and (K, R) with the same K
    * @return collection of (K, O) pairs, where each key comes form either left or right collection and values are produced by projections
    * @since 2.0
    */
  def outerJoin[K:ClassTag, L, R, O:ClassTag]
      (left: Coll[(K, L)], right: Coll[(K, R)])
      (l: ((K,L)) => O, r: ((K,R)) => O, inner: ((K,(L,R))) => O): Coll[(K,O)]
}

