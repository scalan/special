package special.collection

import scalan.{Internal, NeverInline, Reified, RType}
import scalan.RType._
import scalan.util.CollectionUtil

import scala.reflect.ClassTag

object HashUtil {

  def isPrimitiveType[T](value: T): Boolean = value match {
    case Boolean | Byte | Short | Int | Long | Char | Double | Float => true
    case _ => false
  }


  def collHashCode[T: RType](coll: Coll[T]): Int = if (coll == null) 0
  else {
    coll match {
      case cl: Coll[AnyRef] => deepCollHashCode(cl)
      case cl: Coll[Byte] => byteCollHashCode(cl)
      case cl: Coll[Short] => shortCollHashCode(cl)
      case cl: Coll[Int] => intCollHashCode(cl)
      case cl: Coll[Char] => charCollHashCode(cl)
      case cl: Coll[Long] => longCollHashCode(cl)
      case cl: Coll[Float] => floatCollHashCode(cl)
      case cl: Coll[Double] => doubleCollHashCode(cl)
      case cl: Coll[Boolean] => boolCollHashCode(cl)
    }
  }

  def deepCollHashCode(coll: Coll[AnyRef]): Int = if (coll == null) 0
  else {
    var hash = 1
    coll match {
      case cl: ReplColl[AnyRef] => {
        val length = cl.length
        var i = 0

        cl.value match {
          case element: Tuple2[_, _] => {
            var lElementHash = 0
            var rElementHash = 0
            var lHash = 1
            var rHash = 1
//            val element = cl.value.asInstanceOf[Tuple2[_, _]]
            val lElement = element._1
            val rElement = element._2
            if (lElement == null) lElementHash = 0
            else {
              lElementHash = CollectionUtil.hashElement(lElement)
            }
            if (rElement == null) rElementHash = 0
            else {
              rElementHash = CollectionUtil.hashElement(rElement)
            }
            while (i < length) {
              lHash = 31 * lHash + lElementHash
              rHash = 31 * rHash + rElementHash
              i += 1
            }
            hash = lHash * 41 + rHash
          }
          case _ => {
            var elementHash = 0
            val value = cl.value
            if (value == null) elementHash = 0
            else {
              elementHash = CollectionUtil.hashElement(value)
            }
            while (i < length) {
              hash = 31 * hash + elementHash
              i += 1
            }
          }
        }
      }
      case cl: CollOverArray[AnyRef] => {
        hash = CollectionUtil.deepArrayHashCode(cl.toArray)
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def calcPrimitiveCollHashCode[T](arr: Array[T], hashOne: T => Int): Int ={
    if (arr == null) return 0
    var hash: Int = 1
    val length = arr.length
    var i: Int = 0
    while (i < length) {
      val element = arr(i)
      hash = 31 * hash + hashOne(element)

      i += 1
    }
    return hash
  }

  def byteCollHashCode(coll: Coll[Byte]): Int = if (coll == null) return 0
  else {
    var hash: Int = 1
    coll match {
      case cl: CollOverArray[_] => {
        hash = CollectionUtil.byteArrayHashCode(cl.toArray)
      }
      case cl: ReplColl[_] => {
        val length = cl.length
        var i: Int = 0
        val element = cl.value
        while (i < length) {
          hash = 31 * hash + element
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }
  def shortCollHashCode(coll: Coll[Short]): Int = if (coll == null) return 0
  else {
    var hash: Int = 1
    coll match {
      case cl: CollOverArray[_] => {
        hash = CollectionUtil.shortArrayHashCode(cl.toArray)
      }
      case cl: ReplColl[_] => {
        val length = cl.length
        var i: Int = 0
        val element = cl.value
        while (i < length) {
          hash = 31 * hash + element
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }
  def intCollHashCode(coll: Coll[Int]): Int = if (coll == null) return 0
  else {
    var hash: Int = 1
    coll match {
      case cl: CollOverArray[_] => {
        hash = CollectionUtil.intArrayHashCode(cl.toArray)
      }
      case cl: ReplColl[_] => {
        val length = cl.length
        var i: Int = 0
        val element = cl.value
        while (i < length) {
          hash = 31 * hash + element
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }
  def charCollHashCode(coll: Coll[Char]): Int = if (coll == null) return 0
  else {
    var hash: Int = 1
    coll match {
      case cl: CollOverArray[_] => {
        hash = CollectionUtil.charArrayHashCode(cl.toArray)
      }
      case cl: ReplColl[_] => {
        val length = cl.length
        var i: Int = 0
        val element = cl.value
        while (i < length) {
          hash = 31 * hash + element
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def longCollHashCode(coll: Coll[Long]): Int = {
    if (coll == null) return 0
    var hash: Int = 1
    coll match {
      case coll: CollOverArray[_] => {
        hash = CollectionUtil.longArrayHashCode(coll.toArray)
      }
      case coll: ReplColl[_] => {
        val length = coll.length
        var i: Int = 0
        val element = coll.value
        val elementHash = (element ^ element >>> 32).toInt
        while (i < length) {
          hash = 31 * hash + elementHash
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def floatCollHashCode(coll: Coll[Float]): Int = {
    if (coll == null) return 0
    var hash: Int = 1
    coll match {
      case coll: CollOverArray[_] => {
        hash = CollectionUtil.floatArrayHashCode(coll.toArray)
      }
      case coll: ReplColl[_] => {
        val length = coll.length
        var i: Int = 0
        val element = coll.value
        val elementHash = java.lang.Float.floatToIntBits(element)
        while (i < length) {
          hash = 31 * hash + elementHash
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }


  def doubleCollHashCode(coll: Coll[Double]): Int = {
    if (coll == null) return 0
    var hash: Int = 1
    coll match {
      case coll: CollOverArray[_] => {
        hash = CollectionUtil.doubleArrayHashCode(coll.toArray)
      }
      case coll: ReplColl[_] => {
        val length = coll.length
        var i: Int = 0
        val element = coll.value
        val bits = java.lang.Double.doubleToLongBits(element)
        val elementHash = (bits ^ bits >>> 32).toInt
        while (i < length) {
          hash = 31 * hash + elementHash
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def boolCollHashCode(coll: Coll[Boolean]): Int = {
    if (coll == null) return 0
    var hash: Int = 1
    coll match {
      case coll: CollOverArray[_] => {
        hash = CollectionUtil.boolArrayHashCode(coll.toArray)
      }
      case coll: ReplColl[_] => {
        val length = coll.length
        var i: Int = 0
        val element = coll.value
        val elementHash = if (element) 1231 else 1237
        while (i < length) {
          hash = 31 * hash + elementHash
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }
}
