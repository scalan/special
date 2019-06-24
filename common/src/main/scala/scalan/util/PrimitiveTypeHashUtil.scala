package scalan.util

object PrimitiveTypeHashUtil {
  def isPrimitiveType[@specialized T](value: T): Boolean = value match {
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
