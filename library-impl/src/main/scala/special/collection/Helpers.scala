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
    val result = scala.collection.mutable.LinkedHashMap.empty[K, V]
    var i = 0
    while (i < arr.length) {
      val (key, value) = m(arr(i))
      val reducedValue = if (result.contains(key)) r((result(key), value)) else value
      result.update(key, reducedValue)
      i += 1
    }
    val keys = ArrayBuffer.empty[K]
    val values = ArrayBuffer.empty[V]
    for ((k,v) <- result) {
      keys += k
      values += v
    }
    (keys.toArray, values.toArray)
  }
}
