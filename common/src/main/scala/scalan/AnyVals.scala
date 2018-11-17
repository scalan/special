package scalan

import java.util.HashMap

/** Allocation free alternative to scala.Option with similar interface.
  * Using this in recognizers allows:
  * 1) to avoid allocation of Some(x)
  * 2) reading random memory location (where Some is stored) to access x
  **/
class Nullable[+T](val x: T) extends AnyVal {
  @inline def isEmpty = x == null
  @inline def get: T = x
  @inline def isDefined = x != null
  @inline def getOrElse[B >: T](default: =>B): B = if (x != null) x else default
}
object Nullable {
  val None: Nullable[Null] = new Nullable(null)
  def apply[T](x: T): Nullable[T] = new Nullable(x)
  def unapply[T](opt: Nullable[T]): Nullable[T] = opt
}

/** Allocation free alternative to scala.collection.mutable.Map with similar interface.
  * This simplifies optimization of performance critical code. */
class AVHashMap[K,V](val hashMap: HashMap[K,V]) extends AnyVal {
  @inline def get(key: K): Nullable[V] = Nullable(hashMap.get(key))
  @inline def apply(key: K): V = hashMap.get(key)
  @inline def containsKey(key: K): Boolean = hashMap.containsKey(key)
  @inline def put(key: K, value: V): V = hashMap.put(key, value)
  @inline def clear(): Unit = {
    hashMap.clear()
  }
}
object AVHashMap {
  def apply[K,V](initialCapacity: Int) = new AVHashMap[K,V](new HashMap[K,V](initialCapacity))
}

