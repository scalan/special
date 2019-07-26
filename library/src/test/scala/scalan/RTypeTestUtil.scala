package scalan

import spire.syntax.all._

object RTypeTestUtil {
  import scalan.RType._

  def valueMatchesRType[T](value: T, tA: RType[_]): Boolean = tA match {
    case prim: PrimitiveType[a] => value match {
      case b: Byte => prim == ByteType
      case b: Short => prim == ShortType
      case b: Int => prim == IntType
      case b: Long => prim == LongType
      case b: Char => prim == CharType
      case b: Float => prim == FloatType
      case b: Double => prim == DoubleType
      case b: Boolean => prim == BooleanType
      case b: Unit => prim == UnitType
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
