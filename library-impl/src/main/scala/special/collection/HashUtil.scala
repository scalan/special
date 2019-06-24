package special.collection

import scalan.{Internal, NeverInline, Reified, RType}
import scalan.RType._
import scalan.util.{CollectionUtil, PrimitiveTypeHashUtil}

import scala.reflect.ClassTag

object HashUtil {
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
      case cl: CollOverArray[AnyRef] => {
        hash = CollectionUtil.arrayHashCode(cl.toArray)
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
