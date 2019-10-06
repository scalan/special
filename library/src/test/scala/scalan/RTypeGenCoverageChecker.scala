package scalan

import spire.syntax.all.cfor

/**
  * In future we could have some other RType generators,
  * so we will to unify coverage checks through this trait.
  */
trait RTypeGenCoverageChecker {
  /** Take type into consideration.
    *
    * @param    item     RType[_] value to be marked as generated
    */
  def consider(item: RType[_]): Unit

  /** Check if all required types are generated.
    *
    * @param    depth     There could be nested types. This parameter show how deep this nesting should be checked.
    * @return   `true` if every required types has been generated, `false` otherwise.
    */
  def isFullyCovered(depth: Int): Boolean
}

class FullTypeCoverageChecker extends RTypeGenCoverageChecker {
  import RType._
  import special.collection._

  private type TypePosition = (Class[_], Int)
  private var typePositions: Set[TypePosition] = Set.empty

  override def consider(item: RType[_]): Unit = {
    decomposeValue(item)
  }

  override def isFullyCovered(depth: Int): Boolean = {
    val typesForCoverage = Seq(classOf[PrimitiveType[_]], classOf[PairType[_, _]], classOf[ArrayType[_]],
      classOf[CollType[_]], classOf[ReplCollType[_]], classOf[OptionType[_]], StringType.classTag.getClass)
    cfor(0)(_ < depth, _ + 1) { i =>
      for (currentType <- typesForCoverage) {
        if (!typePositions.contains(new TypePosition(currentType, i))) {
          println(s"Type ${currentType} is not found at depth ${i}")
          return false
        }
      }
    }
    true
  }

  private def attachResult(typeName: Class[_], depth: Int) = {
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
    case StringType => attachResult(StringType.classTag.getClass, depth)
    case _ => throw new RuntimeException(s"Unknown generated RType: ${item}")
  }
}
