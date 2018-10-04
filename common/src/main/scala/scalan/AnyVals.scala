package scalan

import java.util.HashMap

/** Allocation free alternative to scala.Option with similar interface */
class ValOpt[+T](val d: T) extends AnyVal {
  @inline def isEmpty = d == null
  @inline def get: T = d
  @inline def isDefined = d != null
  @inline def getOrElse[B >: T](default: =>B): B = if (d != null) d else default
}
object ValOpt {
  val None: ValOpt[Null] = new ValOpt(null)
  def apply[T](x: T): ValOpt[T] = new ValOpt(x)
  def unapply[T](opt: ValOpt[T]): ValOpt[T] = opt
}

/** Allocation free alternative to scala.collection.mutable.Map with similar interface. */
class AVHashMap[K,V](val hashMap: HashMap[K,V]) extends AnyVal {
  @inline def get(key: K): ValOpt[V] = ValOpt(hashMap.get(key))
  @inline def apply(key: K): V = hashMap.get(key)
  @inline def containsKey(key: K): Boolean = hashMap.containsKey(key)
  @inline def put(key: K, value: V): V = hashMap.put(key, value)
}
object AVHashMap {
  def apply[K,V](initialCapacity: Int) = new AVHashMap[K,V](new HashMap[K,V](initialCapacity))
}

