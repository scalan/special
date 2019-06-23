package special.collection

import scalan.{Internal, NeverInline, Reified, RType}
import scalan.RType._
import scalan.util.{CollectionUtil, PrimitiveTypeHashUtil}

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

  def calcPrimitiveCollHashCode[T](coll: Coll[T], hashOne: T => Int): Int = if (coll == null) return 0
  else {
    var hash: Int = 1
    coll match {
      case cl: CollOverArray[_] => {
        hash = CollectionUtil.arrayHashCode(cl.toArray)
      }
      case cl: ReplColl[_] => {
        val length = cl.length
        var i: Int = 0
        val element = cl.value
        val hashElement = hashOne(element)
        while (i < length) {
          hash = 31 * hash + hashElement
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def byteCollHashCode(coll: Coll[Byte]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashByte)
  }

  def shortCollHashCode(coll: Coll[Short]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashShort)
  }

  def intCollHashCode(coll: Coll[Int]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashInt)
  }

  def charCollHashCode(coll: Coll[Char]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashChar)
  }

  def longCollHashCode(coll: Coll[Long]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashLong)
  }

  def floatCollHashCode(coll: Coll[Float]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashFloat)
  }

  def doubleCollHashCode(coll: Coll[Double]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashDouble)
  }

  def boolCollHashCode(coll: Coll[Boolean]): Int = {
    calcPrimitiveCollHashCode(coll, PrimitiveTypeHashUtil.hashBool)
  }
}
