package scalan

import spire.syntax.all.cfor

trait RTypeGenCoverageChecker {
  def consider(item: RType[_]): Unit
  def isFullyCovered(depth: Int): Boolean
}

class BasicTypeCoverageChecker extends RTypeGenCoverageChecker {
  import RType._

  private type TypePosition = (String, Int)
  private var typePositions: Set[TypePosition] = Set.empty

  override def consider(item: RType[_]): Unit = {
    decomposeValue(item)
  }

  override def isFullyCovered(depth: Int): Boolean = {
    val names = Seq("PrimitiveType", "PairType", "ArrayType", "StringType")
    cfor(0)(_ < depth, _ + 1) { i =>
      for (name <- names)
        if (!typePositions.contains(new TypePosition(name, i)))
          false
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
    case (stringType: RType[String]@unchecked) => attachResult("StringType", depth)
    case _ => throw new RuntimeException(s"Unknown generated RType: ${item}")
  }
}