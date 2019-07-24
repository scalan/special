package scalan

import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen}

class GenConfiguration(val maxArrayLength: Int = 100) {}

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

  def primitiveValueGen[T](t: PrimitiveType[T]): Gen[_] = t match {
    case ByteType => choose[Byte](Byte.MinValue, Byte.MaxValue)
    case ShortType => choose[Short](Short.MinValue, Short.MaxValue)
    case IntType => choose[Int](Int.MinValue, Int.MaxValue)
    case CharType => choose[Char](Char.MinValue, Char.MaxValue)
    case LongType => choose[Long](Long.MinValue, Long.MaxValue)
    case FloatType => choose[Float](Float.MinValue, Float.MaxValue)
    case DoubleType => choose[Double](Double.MinValue, Double.MaxValue)
    case BooleanType => Gen.oneOf(true, false)
    case _ => throw new NotImplementedError("Not supported")
  }

  def getArrayGen[T](valGen: Gen[T], count: Int)
                    (implicit evb: Buildable[T,Array[T]], evt: Array[T] => Traversable[T]): Gen[Array[T]] = {
    containerOfN[Array, T](count, valGen)
  }

  def rtypeValueGen[T](t: RType[T], conf: GenConfiguration = new GenConfiguration()): Gen[_] = t match {
    case prim: PrimitiveType[a] => primitiveValueGen(prim)
    case arrayType: ArrayType[a] =>
      getArrayGen(rtypeValueGen(arrayType.tA, conf), conf.maxArrayLength)
    case pairType: PairType[a, b] =>
      for { left <- rtypeValueGen(pairType.tFst); right <- rtypeValueGen(pairType.tSnd) } yield (left, right)
    case (stringType: RType[String]@unchecked) =>
      Gen.asciiPrintableStr
    case _ => throw new NotImplementedError("Not supported")
  }
}
