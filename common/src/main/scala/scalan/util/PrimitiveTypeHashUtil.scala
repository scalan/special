package scalan.util

object PrimitiveTypeHashUtil {
  def isPrimitiveType[T](value: T): Boolean = value match {
    case Boolean | Byte | Short | Int | Long | Char | Double | Float => true
    case _ => false
  }

  def hashPrimitive[T](element: T): Int = element match {
    case e: Byte => hashByte(e)
    case e: Short => hashShort(e)
    case e: Int => hashInt(e)
    case e: Char => hashChar(e)
    case e: Long => hashLong(e)
    case e: Float => hashFloat(e)
    case e: Double => hashDouble(e)
    case e: Boolean => hashBool(e)
    case _ => throw new RuntimeException("Non-primitive type was passed")
  }

  def hashByte(element: Byte) = element.toInt

  def hashShort(element: Short) = element.toInt

  def hashInt(element: Int) = element

  def hashChar(element: Char) = element.toInt

  def hashLong(element: Long) = {
    (element ^ element >>> 32).toInt
  }

  def hashFloat(element: Float) = {
    java.lang.Float.floatToIntBits(element)
  }

  def hashDouble(element: Double) = {
    val bits = java.lang.Double.doubleToLongBits(element)
    (bits ^ bits >>> 32).toInt
  }

  def hashBool(element: Boolean) = if (element) 1231 else 1237
}
