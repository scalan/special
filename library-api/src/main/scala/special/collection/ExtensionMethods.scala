package special.collection

import scala.reflect.ClassTag
import scalan.util.CollectionUtil._

object ExtentionMethods {
  import Helpers._

  implicit class CollOps[A](val source: Coll[A]) extends AnyVal {

//    /** Returns the length of the longest prefix whose elements all satisfy some predicate.
//      *
//      *  $mayNotTerminateInf
//      *
//      *  @param   p     the predicate used to test elements.
//      *  @return  the length of the longest prefix of this $coll
//      *           such that every element of the segment satisfies the predicate `p`.
//      */
//    def prefixLength(p: A => Boolean): Int = xs.segmentLength(p, 0)
//
//    /** Finds index of first occurrence of some value in this $coll after or at some start index.
//      *
//      *  @param   elem   the element value to search for.
//      *  @param   from   the start index
//      *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
//      *           to `elem`, or `-1`, if none exists.
//      */
//    def indexOf(elem: A, from: Int): Int = xs.indexWhere(elem == _, from)
//
//    /** Finds index of last occurrence of some value in this $coll before or at a given end index.
//      *
//      *  @param   elem   the element value to search for.
//      *  @param   end    the end index.
//      *  @return  the index `<= end` of the last element of this $coll that is equal (as determined by `==`)
//      *           to `elem`, or `-1`, if none exists.
//      */
//    def lastIndexOf(elem: A, end: Int): Int = xs.lastIndexWhere(elem == _, end)
//
//    /** Finds index of last element satisfying some predicate.
//      *
//      *  @param   p     the predicate used to test elements.
//      *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
//      *           or `-1`, if none exists.
//      */
//    def lastIndexWhere(p: A => Boolean): Int = xs.lastIndexWhere(p, xs.length - 1)
//

//    /** Builds a new $coll from this $coll without any duplicate elements.
//      *
//      *  @return  A new $coll which contains the first occurrence of every element of this $coll.
//      */
//    def distinct: Coll[A] = {
//      xs.unionSets(xs.builder.fromItems())
//    }
//
//    /** Tests whether this $coll starts with the given sequence.
//      *
//      * @param  that    the sequence to test
//      * @return `true` if this collection has `that` as a prefix, `false` otherwise.
//      */
////    def startsWith[B](that: Coll[B]): Boolean = startsWith(that, 0)
//
//    /** Tests whether this $coll contains the given sequence at a given index.
//      *
//      * '''Note''': If the both the receiver object `this` and the argument
//      * `that` are infinite sequences this method may not terminate.
//      *
//      * @param  that    the sequence to test
//      * @param  offset  the index where the sequence is searched.
//      * @return `true` if the sequence `that` is contained in this $coll at
//      *         index `offset`, otherwise `false`.
//      */
////    def startsWith[B](that: Coll[B], offset: Int): Boolean =
//
//    /** Tests whether this $coll ends with the given collection.
//      *  @param  that    the collection to test
//      *  @return `true` if this $coll has `that` as a suffix, `false` otherwise.
//      */
////    def endsWith(that: Coll[A]): Boolean
//
//    /** A copy of this $coll with an element value appended until a given target length is reached.
//      *
//      *  @param   len   the target length
//      *  @param   elem  the padding value
//      *  @return a new collection consisting of all elements of this $coll followed by the minimal
//      *          number of occurrences of `elem` so that the resulting collection has a length of at least `len`.
//      */
//    def padTo(len: Int, elem: A): Coll[A] = {
//      if (len <= xs.length) xs
//      else
//        xs.append(xs.builder.replicate(len - xs.length, elem))
//    }

  }
  implicit class PairCollOps[A: ClassTag, B: ClassTag](source: Coll[(A,B)]) {
    // TODO optimize
    def unionSetByKey(that: Coll[(A,B)]): Coll[(A,B)] = {
      source.unionSetByKey(that)
    }

    def reduceByKey(r: ((B,B)) => B): Coll[(A,B)] = {
      source.mapReduce(identity, r)
    }

    def sumByKey(implicit m: Monoid[B]): Coll[(A,B)] =
      reduceByKey(r => m.plus(r._1, r._2))
  }
}

