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

  def pairTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[PairType[_, _]] = depth match {
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

  def getPairAnyTypeGen(gn: Gen[RType[_]], depth: Int): Gen[RType[_]] = pairTypeGen(gn, depth)

  def arrayTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = {
    for { item <- itemGen } yield new ArrayType(item)
  }

  def arrayTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[ArrayType[_]] = depth match {
    case 1 =>
      arrayTypeGenFinal(itemGen)
    case _ =>
      Gen.oneOf(
        arrayTypeGenFinal(itemGen),
        arrayTypeGenFinal(arrayTypeGen(itemGen, depth - 1))
      )
  }

  def getAnyArrayGen(gn: Gen[RType[_]], depth: Int): Gen[RType[_]] = arrayTypeGen(gn, depth)

  /*
   * Full type generators
   */
  def getBasicTypeGen(depth: Int): Gen[RType[_]] = depth match {
    case 1 => Gen.oneOf(dataTypeGen, getPairAnyTypeGen(dataTypeGen, 1), getAnyArrayGen(dataTypeGen, 1))
    case _ =>
      Gen.oneOf(dataTypeGen, getPairAnyTypeGen(getBasicTypeGen(depth - 1), 1), getAnyArrayGen(getBasicTypeGen(depth - 1), 1))
  }

  def getPrimitiveArrayGen(depth: Int) = getAnyArrayGen(primitiveTypeGen, depth)
  def getArrayGen(depth: Int) = getAnyArrayGen(getBasicTypeGen(depth - 1), 1)

  def getPrimitivePairGen(depth: Int) = getPairAnyTypeGen(primitiveTypeGen, depth)
  def getPairGen(depth: Int) = getPairAnyTypeGen(getBasicTypeGen(depth - 1), 1)
}
