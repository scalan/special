package scalan

import spire.syntax.all._
import spire.syntax.trig.trigOps

object RTypeUtil {
  import scalan.RType._
  import special.collection._

  def clone[T](value: T)(implicit t: RType[T]): T = t match {
    case prim: PrimitiveType[a] => value
    case arrayType: ArrayType[a] =>
      val array = value.asInstanceOf[Array[a]]
      var copy = Array.ofDim[a](array.length)(arrayType.tA.classTag)
      cfor(0)(_ < array.length, _ + 1) { i =>
        copy(i) = clone(array(i))(arrayType.tA).asInstanceOf[a]
      }
      copy.asInstanceOf[T]
    case pairType: PairType[a, b] =>
      val pair = value.asInstanceOf[Tuple2[a, b]]
      return (clone(pair._1)(pairType.tFst), clone(pair._2)(pairType.tSnd)).asInstanceOf[T]
    case optionType: OptionType[a] =>
      val option = value.asInstanceOf[Option[a]]
      val cloned = if (option.isDefined) Some(clone(option.get)(optionType.tA)) else None
      cloned.asInstanceOf[T]
    case replCollType: ReplCollType[a] =>
      val coll = value.asInstanceOf[ReplColl[a]]
      val cloned = clone(coll.value)(replCollType.tItem)
      (new CReplColl(cloned, coll.length)(replCollType.tItem)).asInstanceOf[T]
    case collType: CollType[a] =>
      val coll = value.asInstanceOf[Coll[a]]
      val cloned = clone(coll.toArray)(ArrayType(collType.tItem))
      coll.builder.fromArray(cloned)(collType.tItem).asInstanceOf[T]
    case StringType =>
      val arr = value.asInstanceOf[String].toArray
      arr.mkString.asInstanceOf[T]
    case _ => throw new RuntimeException(s"Can't clone ${t}.")
  }
}
