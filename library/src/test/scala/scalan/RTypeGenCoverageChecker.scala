package scalan

import spire.syntax.all.cfor

trait RTypeGenCoverageChecker {
  def consider(item: RType[_]): Unit
  def isFullyCovered(depth: Int): Boolean
}

class FullTypeCoverageChecker extends RTypeGenCoverageChecker {
  import RType._
  import special.collection._

  private type TypePosition = (String, Int)
  private var typePositions: Set[TypePosition] = Set.empty

  override def consider(item: RType[_]): Unit = {
    decomposeValue(item)
  }

  override def isFullyCovered(depth: Int): Boolean = {
    val names = Seq("PrimitiveType", "PairType", "ArrayType", "StringType", "CollType", "ReplCollType", "OptionType")
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

  private def attachResult(typeName: String, depth: Int) = {
    val newTypePosition: TypePosition = (typeName, depth)
    typePositions = typePositions + newTypePosition
  }

  private def decomposeValue(item: RType[_], depth: Int = 0): Unit = item match {
    case prim: PrimitiveType[a] => attachResult("PrimitiveType", depth)
    case pair: PairType[a, b]@unchecked =>
      attachResult("PairType", depth)
      decomposeValue(pair.tFst, depth + 1)
      decomposeValue(pair.tSnd, depth + 1)
    case array: ArrayType[a] =>
      attachResult("ArrayType", depth)
      decomposeValue(array.tA, depth + 1)
    case opt: OptionType[a] =>
      attachResult("OptionType", depth)
      decomposeValue(opt.tA, depth + 1)
    case coll: CollType[a] =>
      attachResult("CollType", depth)
      decomposeValue(coll.tItem, depth + 1)
    case replColl: ReplCollType[a] =>
      attachResult("ReplCollType", depth)
      decomposeValue(replColl.tItem, depth + 1)
    case StringType => attachResult("StringType", depth)
    case _ => throw new RuntimeException(s"Unknown generated RType: ${item}")
  }
}
