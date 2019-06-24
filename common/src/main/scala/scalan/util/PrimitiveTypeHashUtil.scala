package scalan.util

import scalan.RType

object PrimitiveTypeHashUtil {

  def hashPrimitive[@specialized T: RType](element: T): Int = RType[T] match {
    case prim: RType.PrimitiveType[a] => {
      prim match {
        case RType.ByteType => hashByte(element.asInstanceOf[Byte])
        case RType.ShortType => hashShort(element.asInstanceOf[Short])
        case RType.IntType => hashInt(element.asInstanceOf[Int])
        case RType.CharType => hashChar(element.asInstanceOf[Char])
        case RType.LongType => hashLong(element.asInstanceOf[Long])
        case RType.FloatType => hashFloat(element.asInstanceOf[Float])
        case RType.DoubleType => hashDouble(element.asInstanceOf[Double])
        case RType.BooleanType => hashBool(element.asInstanceOf[Boolean])
      }
    }
    case _ => throw new RuntimeException("Non-primitive type was passed")
  }

  def hashByte(element: Byte): Int = element.toInt

  def hashShort(element: Short): Int = element.toInt

  def hashInt(element: Int): Int = element

  def hashChar(element: Char): Int = element.toInt

  def hashLong(element: Long): Int = {
    (element ^ element >>> 32).toInt
  }

  def hashFloat(element: Float): Int = {
    java.lang.Float.floatToIntBits(element)
  }

  def hashDouble(element: Double): Int = {
    val bits = java.lang.Double.doubleToLongBits(element)
    (bits ^ bits >>> 32).toInt
  }

  def hashBool(element: Boolean): Int = if (element) 1231 else 1237
}
