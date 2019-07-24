package scalan

import spire.syntax.all.cfor

trait RTypeGenCoverageChecker {
  def encounter(item: RType[_]): Unit
  def isCovered(depth: Int): Boolean
}

class RTypeFullGenCoverageChecker extends RTypeGenCoverageChecker {
  import RType._

  private var usedPlaces: Set[(String, Int)] = Set.empty

  override def encounter(item: RType[_]): Unit = {
    parseValue(item)
  }

  override def isCovered(depth: Int): Boolean = {
    val names = Seq("PrimitiveType", "PairType", "ArrayType", "StringType")
    cfor(0)(_ < depth, _ + 1) { i =>
      for (name <- names)
        if (!usedPlaces.contains((name, i)))
          false
    }
    true
  }

  private def attachResult(item: String, depth: Int) = {
    val newItem = (item, depth)
    usedPlaces = usedPlaces + newItem
  }

  private def parseValue(item: RType[_], depth: Int = 0): Unit = item match {
    case prim: PrimitiveType[a] => attachResult("PrimitiveType", depth)
    case pair: PairType[a, b]@unchecked =>
      attachResult("PairType", depth)
      parseValue(pair.tFst, depth + 1)
      parseValue(pair.tSnd, depth + 1)
    case array: ArrayType[a] =>
      attachResult("ArrayType", depth)
      parseValue(array.tA, depth + 1)
    case (stringType: RType[String]@unchecked) => attachResult("StringType", depth)
    case _ => throw new RuntimeException(s"Unknown generated RType: ${item}")
  }
}