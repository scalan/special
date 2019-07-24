package scalan

import org.scalacheck.{Arbitrary, Gen}

trait RTypeGens {
  import Gen._
  import RType._

  /*
   * Primitive and simple type generators
   */

  val primitiveTypeGen = Gen.oneOf[RType[_]](BooleanType, ByteType, ShortType,
    IntType, LongType, CharType, FloatType, DoubleType)

  val primitiveTypeWithUnitGen = Gen.oneOf[RType[_]](primitiveTypeGen, UnitType)

  val dataTypeGen = Gen.oneOf[RType[_]](primitiveTypeGen, StringType)

  /*
   */

  def pairTypeGenFinal(itemGenLeft: Gen[RType[_]], itemGenRight: Gen[RType[_]]): Gen[PairType[_, _]] = {
    for { left <- itemGenLeft; right <- itemGenRight } yield new PairType(left, right)
  }

  def pairTypeGen(itemGen: Gen[RType[_]], depth: Int = 1): Gen[PairType[_, _]] = depth match {
    case 1 =>
      pairTypeGenFinal(itemGen, itemGen)
    case _ =>
      val lg = pairTypeGen(itemGen, depth - 1)
      val rg = pairTypeGen(itemGen, depth - 1)
      Gen.oneOf(
        pairTypeGenFinal(itemGen, itemGen),
        pairTypeGenFinal(lg, itemGen),
        pairTypeGenFinal(itemGen, rg),
        pairTypeGenFinal(lg, rg),
      )
  }

  def getPairAnyTypeGen(gn: Gen[RType[_]], depth: Int = 4): Gen[RType[_]] = pairTypeGen(gn, depth)

  def arrayTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = {
    for { item <- itemGen } yield new ArrayType(item)
  }

  def arrayTypeGen(itemGen: Gen[RType[_]], depth: Int = 1): Gen[ArrayType[_]] = depth match {
    case 1 =>
      arrayTypeGenFinal(itemGen)
    case _ =>
      Gen.oneOf(
        arrayTypeGenFinal(itemGen),
        arrayTypeGenFinal(arrayTypeGen(itemGen, depth - 1))
      )
  }

  def getArrayAnyTypeGen(gn: Gen[RType[_]], depth: Int = 4): Gen[RType[_]] = arrayTypeGen(gn, depth)

  /*
   * Full type generators
   */
  def getFullTypeGen(depth: Int = 4): Gen[RType[_]] = depth match {
    case 1 => Gen.oneOf(dataTypeGen, getPairAnyTypeGen(dataTypeGen, 1), getArrayAnyTypeGen(dataTypeGen, 1))
    case _ =>
      Gen.oneOf(dataTypeGen, getPairAnyTypeGen(getFullTypeGen(depth - 1), 1), getArrayAnyTypeGen(getFullTypeGen(depth - 1), 1))
  }

  val fullTypeGen = getFullTypeGen(3)

  def getArrayPrimitiveTypeGen(depth: Int = 4) = getArrayAnyTypeGen(primitiveTypeGen, depth)
  def getArrayTypeGen(depth: Int = 4) = getArrayAnyTypeGen(getFullTypeGen(depth - 1), 1)

  def getPairPrimitiveTypeGen(depth: Int = 4) = getPairAnyTypeGen(primitiveTypeGen, depth)
  def getPairTypeGen(depth: Int = 4) = getPairAnyTypeGen(getFullTypeGen(depth - 1), 1)

  val arrayPrimitiveTypeGen = getArrayPrimitiveTypeGen()
  val arrayTypeGen = getArrayTypeGen()
  val pairPrimitiveTypeGen = getPairPrimitiveTypeGen()
  val pairTypeGen = getPairTypeGen()
}
