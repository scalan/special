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

  val arrayPrimitiveTypeGen = arrayTypeGen(4, primitiveTypeGen)
  val arrayDataTypeGen = arrayTypeGen(4, Gen.oneOf(dataTypeGen, pairDataTypeGen))
  val fullDataTypeGen = Gen.oneOf(dataTypeGen, pairDataTypeGen, arrayDataTypeGen)
}
