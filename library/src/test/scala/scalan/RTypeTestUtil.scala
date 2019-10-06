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
      case coll: ReplColl[_] => coll.tItem == replCollType.tItem && coll.forall(item => valueMatchesRType(item, replCollType.tItem))
      case _ => false
    }
    case optionType: OptionType[a] => value match {
      case op: Option[_] => op.forall(item => valueMatchesRType(item, optionType.tA))
      case _ => false
    }

    case _ => false
  }

  def deepEqualityChecker[A](value: A, copy: A)(implicit tA: RType[A]): Boolean = tA match {
    case arrayType: ArrayType[a] =>
      val valInstance = value.asInstanceOf[Array[a]]
      val copyInstance = copy.asInstanceOf[Array[a]]
      if (valInstance.length != copyInstance.length) return false
      cfor(0)(_ < valInstance.length, _ + 1) { i =>
        if (!deepEqualityChecker(valInstance(i), copyInstance(i))(arrayType.tA))
          return false
      }
      true
    case prim: PrimitiveType[a] =>
      copy == value
    case pairType: PairType[a, b] =>
      val valInstance = value.asInstanceOf[Tuple2[a, b]]
      val copyInstance = copy.asInstanceOf[Tuple2[a, b]]
      deepEqualityChecker(valInstance._1, copyInstance._1)(pairType.tFst) && deepEqualityChecker(valInstance._2, copyInstance._2)(pairType.tSnd)
    case StringType =>
      copy == value
    case opt: OptionType[a] =>
      val copyOpt = copy.asInstanceOf[Option[a]]
      val valueOpt = value.asInstanceOf[Option[a]]
      if (copyOpt.isDefined != valueOpt.isDefined) return false
      if (copyOpt.isDefined) deepEqualityChecker(copyOpt.get, valueOpt.get)(opt.tA) else true
    case coll: ReplCollType[a] =>
      val copyColl = copy.asInstanceOf[ReplColl[a]]
      val valueColl = value.asInstanceOf[ReplColl[a]]
      copyColl.length == valueColl.length && deepEqualityChecker(valueColl.value, copyColl.value)(coll.tItem)
    case coll: CollType[a] =>
      val copyColl = copy.asInstanceOf[Coll[a]]
      val valueColl = value.asInstanceOf[Coll[a]]
      if (copyColl.length != valueColl.length) return false
      cfor(0)(_ < valueColl.length, _ + 1) { i =>
        if (!deepEqualityChecker(valueColl(i), copyColl(i))(coll.tItem))
          return false
      }
      true
    case _ => copy == value
  }
}
