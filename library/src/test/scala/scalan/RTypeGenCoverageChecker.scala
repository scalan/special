package scalan

import java.io.Serializable
import spire.syntax.all.cfor

/**
  * In future we could have some other RType generators,
  * so we will to unify coverage checks through this trait.
  */
trait RTypeGenCoverageChecker {
  /** Take type into consideration.
    *
    * @param    item     RType[_] value to be considered
    */
  def consider(item: RType[_]): Unit

  /** Check if all required types are generated.
    *
    * @param    depth     depth of check
    * @return   `true` if every required types has been generated, `false` otherwise.
    */
  def isFullyCovered(depth: Int): Boolean
}

class FullTypeCoverageChecker extends RTypeGenCoverageChecker {
  import RType._
  import special.collection._

  private type TypePosition = (Serializable, Int)
  private var typePositions: Set[TypePosition] = Set.empty

  override def consider(item: RType[_]): Unit = {
    decomposeValue(item)
  }

  override def isFullyCovered(depth: Int): Boolean = {
    val names = Seq(classOf[PrimitiveType[_]], classOf[PairType[_, _]], classOf[ArrayType[_]],
      classOf[CollType[_]], classOf[ReplCollType[_]], classOf[OptionType[_]], StringType)
    cfor(0)(_ < depth, _ + 1) { i =>
      for (name <- names) {
        if (!typePositions.contains(new TypePosition(name, i))) {
          println(s"Type ${name} is not found at depth ${i}")
          return false
        }
      }
    }
    true
  }

  private def attachResult(typeName: Serializable, depth: Int) = {
    val newTypePosition: TypePosition = (typeName, depth)
    typePositions = typePositions + newTypePosition
  }

  private def decomposeValue(item: RType[_], depth: Int = 0): Unit = item match {
    case prim: PrimitiveType[a] => attachResult(classOf[PrimitiveType[_]], depth)
    case pair: PairType[a, b]@unchecked =>
      attachResult(classOf[PairType[_, _]], depth)
      decomposeValue(pair.tFst, depth + 1)
      decomposeValue(pair.tSnd, depth + 1)
    case array: ArrayType[a] =>
      attachResult(classOf[ArrayType[_]], depth)
      decomposeValue(array.tA, depth + 1)
    case opt: OptionType[a] =>
      attachResult(classOf[OptionType[_]], depth)
      decomposeValue(opt.tA, depth + 1)
    case coll: CollType[a] =>
      attachResult(classOf[CollType[_]], depth)
      decomposeValue(coll.tItem, depth + 1)
    case replColl: ReplCollType[a] =>
      attachResult(classOf[ReplCollType[_]], depth)
      decomposeValue(replColl.tItem, depth + 1)
    case StringType => attachResult(StringType, depth)
    case _ => throw new RuntimeException(s"Unknown generated RType: ${item}")
  }
}
