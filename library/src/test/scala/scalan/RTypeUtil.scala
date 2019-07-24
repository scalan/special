package scalan

import scalan.RType.{ArrayType, PairType, PrimitiveType}

object RTypeUtil {
  def valueMatchesRType[T](value: T, tA: RType[_]): Boolean = tA match {
    case prim: PrimitiveType[a] => value match {
      case b: Byte => true
      case b: Short => true
      case b: Int => true
      case b: Long => true
      case b: Char => true
      case b: Float => true
      case b: Double => true
      case b: Boolean => true
      case b: Unit => true
      case _ => false
    }
    case arrayType: ArrayType[a] => value match {
      case arr: Array[_] => if (arr.length != 0) valueMatchesRType(arr(0), arrayType.tA) else true
      case _ => false
    }
    case pairType: PairType[a, b] => value match {
      case pair: Tuple2[_, _] => valueMatchesRType(pair._1, pairType.tFst) && valueMatchesRType(pair._2, pairType.tSnd)
      case _ => false
    }
    case (stringType: RType[String]@unchecked) => value match {
      case str: String => true
      case _ => false
    }

    case _ => false
  }
}
