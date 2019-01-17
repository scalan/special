package special.collection

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Helpers {
  private def sameLengthErrorMsg[A,B](xs: Coll[A], ys: Coll[B]) =
    s"Collections should have same length but was ${xs.length} and ${ys.length}:\n xs=$xs;\n ys=$ys"

  def assertSameLength[A,B](xs: Coll[A], ys: Coll[B]) = {
    assert(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  def requireSameLength[A,B](xs: Coll[A], ys: Coll[B]) = {
    require(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  def mapReduce[A, K: ClassTag, V: ClassTag](arr: Array[A], m: A => (K, V), r: ((V, V)) => V): (Array[K], Array[V]) = {
    val keyPositions = new java.util.HashMap[K, Int](32)
    val keys = ArrayBuffer.empty[K]
    val values = ArrayBuffer.empty[V]
    var i = 0
    while (i < arr.length) {
      val (key, value) = m(arr(i))
      val pos = keyPositions.getOrDefault(key, 0)
      if (pos == 0) {
        keyPositions.put(key, keys.length + 1)
        keys += key
        values += value
      } else {
        values(pos - 1) = r((values(pos - 1), value))
      }
      i += 1
    }
    (keys.toArray, values.toArray)
  }

  def mapToArrays[K: ClassTag, V: ClassTag](m: Map[K,V]): (Array[K], Array[V]) = {
    val keys = ArrayBuffer.empty[K]
    val values = ArrayBuffer.empty[V]
    for ((k,v) <- m) {
      keys += k
      values += v
    }
    (keys.toArray, values.toArray)
  }

  type TokenId = Coll[Byte]
  type TokensColl = Coll[(TokenId, Long)]

  implicit class PairCollOps[L,R](val source: Coll[(L,R)]) extends AnyVal {
  }
}
