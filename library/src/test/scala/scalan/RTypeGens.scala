package scalan

import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen}

class GenConfiguration(val maxArrayLength: Int = 100) {}

trait RTypeGens {
  import Gen._
  import RType._

  val primitiveTypeGen = Gen.oneOf[RType[_]](BooleanType, ByteType, ShortType,
    IntType, LongType, CharType, FloatType, DoubleType)

  val primitiveTypeGenWithUnit = Gen.oneOf[RType[_]](primitiveTypeGen, UnitType)

  val dataTypeGen = Gen.oneOf[RType[_]](primitiveTypeGen, StringType)

  def pairTypeGenFinal(itemGenLeft: Gen[RType[_]], itemGenRight: Gen[RType[_]]): Gen[PairType[_, _]] = {
    for { left <- itemGenLeft; right <- itemGenRight } yield new PairType(left, right)
  }

  def pairTypeGen(depth: Int = 1, itemGen: Gen[RType[_]]): Gen[PairType[_, _]] = depth match {
    case 1 =>
      pairTypeGenFinal(itemGen, itemGen)
    case _ => pairTypeGen(depth - 1, itemGen) match {
      case lg: Gen[PairType[_, _]]@unchecked => {
        pairTypeGen(depth - 1, itemGen) match {
          case rg: Gen[PairType[_, _]]@unchecked => {
            Gen.oneOf(
              pairTypeGenFinal(itemGen, itemGen),
              pairTypeGenFinal(lg, itemGen),
              pairTypeGenFinal(itemGen, rg),
              pairTypeGenFinal(lg, rg),
            )
          }
          case _ => throw new RuntimeException("Invalid rGen")
        }
      }
      case _ => throw new RuntimeException("Invalid lGen")
    }
  }


  val pairPrimitiveTypeGen = pairTypeGen(4, primitiveTypeGen)
  val pairDataTypeGen = pairTypeGen(4, dataTypeGen)

  def arrayTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = {
    for { item <- itemGen } yield new ArrayType(item)
  }

  def arrayTypeGen(depth: Int = 1, itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = depth match {
    case 1 =>
      arrayTypeGenFinal(itemGen)
    case _ => arrayTypeGen(depth - 1, itemGen) match {
      case rg: Gen[PairType[_, _]]@unchecked => {
        Gen.oneOf(
          arrayTypeGenFinal(itemGen),
          arrayTypeGenFinal(rg)
        )
      }
      case _ => throw new RuntimeException("Invalid rGen")
    }
  }

  val arrayPrimitiveTypeGen = arrayTypeGen(1, primitiveTypeGen)
  val arrayDataTypeGen = arrayTypeGen(1, Gen.oneOf(dataTypeGen, pairDataTypeGen))
  val fullDataTypeGen = Gen.oneOf(dataTypeGen, pairDataTypeGen, arrayDataTypeGen)

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
