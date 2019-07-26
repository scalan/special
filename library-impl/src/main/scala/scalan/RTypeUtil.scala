package scalan

import spire.syntax.all._
import spire.syntax.trig.trigOps

object RTypeUtil {
  import scalan.RType._

  def clone[T](t: RType[_], value: T): Any = t match {
    case prim: PrimitiveType[a] => value
    case arrayType: ArrayType[a] =>
      val array = value.asInstanceOf[Array[a]]
      var copy = Array.ofDim[a](array.length)(arrayType.tA.classTag)
      cfor(0)(_ < array.length, _ + 1) { i =>
        copy(i) = clone(arrayType.tA, array(i)).asInstanceOf[a]
      }
      copy
    case pairType: PairType[a, b] =>
      val instanced = value.asInstanceOf[Tuple2[a, b]]
      return (clone(pairType.tFst, instanced._1), clone(pairType.tSnd, instanced._2))
    case (stringType: RType[String]@unchecked) =>
      val arr = value.asInstanceOf[String].toArray
      arr.mkString
    case _ => throw new NotImplementedError("Not supported")
  }
}
