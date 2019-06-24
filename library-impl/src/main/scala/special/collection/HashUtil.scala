package special.collection

import scalan.{Internal, NeverInline, Reified, RType}
import scalan.RType._
import scalan.util.{CollectionUtil, PrimitiveTypeHashUtil}

import scala.reflect.ClassTag

object HashUtil {
  def collHashCode[T: RType](coll: Coll[T]): Int = if (coll == null) 0
  else {
    deepCollHashCode(coll)
  }

  def calcPrimitiveCollHashCode[T](coll: Coll[T], hashT: T => Int): Int = if (coll == null) return 0
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
        val hashElement = hashT(element)
        while (i < length) {
          hash = 31 * hash + hashElement
          i += 1
        }
      }
      case _ => throw new NotImplementedError(s"Unsupported collection passed to deepHashCode: $coll")
    }
    hash
  }

  def deepCollHashCode[T](coll: Coll[T]): Int = {
    calcPrimitiveCollHashCode(coll, CollectionUtil.hashOne)
  }
}
