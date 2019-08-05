package scalan

import spire.syntax.all._
import spire.syntax.trig.trigOps

object RTypeUtil {
  import scalan.RType._
  import special.collection._

  def clone[T](t: RType[_], value: T): T = t match {
    case prim: PrimitiveType[a] => value
    case arrayType: ArrayType[a] =>
      val array = value.asInstanceOf[Array[a]]
      var copy = Array.ofDim[a](array.length)(arrayType.tA.classTag)
      cfor(0)(_ < array.length, _ + 1) { i =>
        copy(i) = clone(arrayType.tA, array(i)).asInstanceOf[a]
      }
      copy.asInstanceOf[T]
    case pairType: PairType[a, b] =>
      val pair = value.asInstanceOf[Tuple2[a, b]]
      return (clone(pairType.tFst, pair._1), clone(pairType.tSnd, pair._2)).asInstanceOf[T]
    case optionType: OptionType[a] =>
      val option = value.asInstanceOf[Option[a]]
      val res = if (option.isDefined) Some(clone(optionType.tA, option.get)) else None
      res.asInstanceOf[T]
    case replCollType: ReplCollType[a] =>
      val coll = value.asInstanceOf[ReplColl[a]]
      val cloned = clone(replCollType.tItem, coll.value)
      (new CReplColl(cloned, coll.length)(replCollType.tItem)).asInstanceOf[T]
    case collType: CollType[a] =>
      val coll = value.asInstanceOf[Coll[a]]
      val cloned = clone(new ArrayType(collType.tItem), coll.toArray)
      coll.builder.fromArray(cloned)(collType.tItem).asInstanceOf[T]
    case StringType =>
      val arr = value.asInstanceOf[String].toArray
      arr.mkString.asInstanceOf[T]
    case _ => throw new RuntimeException(s"Can't clone ${t}.")
  }
}
