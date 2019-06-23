package scalan.util

object PrimitiveTypeHashUtil {
  def hashByte(element: Byte) => element.toInt

  def hashShort(element: Short) => element.toInt

  def hashInt(element: Int) => element

  def hashChar(element: Char) => element.toInt

  def hashLong(element: Long) => {
    (element ^ element >>> 32).toInt
  }

  def hashFloat(element: Float) => {
    java.lang.Float.floatToIntBits(element)
  }

  def hashDouble(element: Double) => {
    val bits = java.lang.Double.doubleToLongBits(element)
    (bits ^ bits >>> 32).toInt
  }

  def hashBool(element: Boolean) => if (element) 1231 else 1237
}
