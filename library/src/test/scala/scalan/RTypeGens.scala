package scalan

import org.scalacheck.{Arbitrary, Gen}

trait RTypeGens {
  import Gen._
  import RType._

  val primitiveNonUnitTypeGen = Gen.oneOf[RType[_]](BooleanType, ByteType, ShortType,
    IntType, LongType, CharType, FloatType, DoubleType)

  val primitiveTypeGen = Gen.oneOf[RType[_]](primitiveNonUnitTypeGen, UnitType)

  val dataTypeGen = Gen.oneOf[RType[_]](primitiveTypeGen, StringType)

  def pairTypeGenFinal(itemGenLeft: Gen[RType[_]], itemGenRight: Gen[RType[_]]): Gen[PairType[_, _]] = {
    for { left <- itemGenLeft; right <- itemGenRight } yield new PairType(left, right)
  }

  def pairTypeGen(depth: Int = 1, itemGen: Gen[RType[_]]): Gen[PairType[_, _]] = depth match {
    case 1 =>
      pairTypeGenFinal(itemGen, itemGen)
    case _ =>
      val lg = pairTypeGen(depth - 1, itemGen)
      val rg = pairTypeGen(depth - 1, itemGen)
      Gen.oneOf(
        pairTypeGenFinal(itemGen, itemGen),
        pairTypeGenFinal(lg, itemGen),
        pairTypeGenFinal(itemGen, rg),
        pairTypeGenFinal(lg, rg),
      )
  }

  def getPairGen(depth: Int = 4, gn: Gen[RType[_]]) = pairTypeGen(depth, gn)
  def getPairDataTypeGen(depth: Int = 4) = pairTypeGen(depth, dataTypeGen)

  def arrayTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = {
    for { item <- itemGen } yield new ArrayType(item)
  }

  def arrayTypeGen(depth: Int = 1, itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = depth match {
    case 1 =>
      arrayTypeGenFinal(itemGen)
    case _ =>
      Gen.oneOf(
        arrayTypeGenFinal(itemGen),
        arrayTypeGenFinal(arrayTypeGen(depth - 1, itemGen))
      )
  }

  val getArrayPrimitiveTypeGen = arrayTypeGen(4, primitiveTypeGen)
  val arrayDataTypeGen = arrayTypeGen(4, Gen.oneOf(dataTypeGen, getPairDataTypeGen()))
  val fullDataTypeGen = Gen.oneOf(dataTypeGen, getPairDataTypeGen(), arrayDataTypeGen)
}
