package scalan

import special.collection.CollType
import spire.syntax.all._

object RTypeTestUtil {
  import scalan.RType._
  import special.collection._

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
      case arr: Array[_] => arr.forall(item => valueMatchesRType(item, arrayType.tA))
      case _ => false
    }
    case pairType: PairType[a, b] => value match {
      case pair: Tuple2[_, _] => valueMatchesRType(pair._1, pairType.tFst) && valueMatchesRType(pair._2, pairType.tSnd)
      case _ => false
    }
    case StringType => value match {
      case str: String => true
      case _ => false
    }
    case collType: CollType[a] => value match {
      case coll: Coll[_] => coll.tItem == collType.tItem && coll.forall(item => valueMatchesRType(item, collType.tItem))
      case _ => false
    }
    case replCollType: ReplCollType[a] => value match {
      case coll: Coll[_] => coll.tItem == replCollType.tItem && coll.forall(item => valueMatchesRType(item, replCollType.tItem))
      case _ => false
    }
    case optionType: OptionType[a] => value match {
      case op: Option[_] => op.forall(item => valueMatchesRType(item, optionType.tA))
      case _ => false
    }

    case _ => false
  }
}
