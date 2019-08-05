package scalan

import org.scalacheck.{Arbitrary, Gen}

trait RTypeGens {
  import Gen._
  import RType._
  import special.collection._

  /*
   * There're three generators for primitive types, since from one side, we want to have just numeric/char types,
   * sometimes there's a need to put UnitType (seldom), sometimes not, that's why additional generator has been made.
   */
  val primitiveTypeGen = Gen.oneOf[RType[_]](BooleanType, ByteType, ShortType,
    IntType, LongType, CharType, FloatType, DoubleType)

  val primitiveTypeWithUnitGen = Gen.oneOf[RType[_]](primitiveTypeGen, UnitType)

  // The reason why StringType is distiguished is that in type hierarchy StringType consists of Chars
  val dataTypeGen = Gen.oneOf[RType[_]](primitiveTypeGen, StringType)

  def pairTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[PairType[_, _]] = {
    def pairTypeGenFinal(itemGenLeft: Gen[RType[_]], itemGenRight: Gen[RType[_]]): Gen[PairType[_, _]] = {
      for { left <- itemGenLeft; right <- itemGenRight } yield new PairType(left, right)
    }
    depth match {
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
  }

  def arrayTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[ArrayType[_]] = {
    def arrayTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ArrayType[_]] = {
      for { item <- itemGen } yield new ArrayType(item)
    }
    depth match {
      case 1 =>
        arrayTypeGenFinal(itemGen)
      case _ =>
        Gen.oneOf(
          arrayTypeGenFinal(itemGen),
          arrayTypeGenFinal(arrayTypeGen(itemGen, depth - 1))
        )
    }
  }

  def getFullTypeGen(depth: Int): Gen[RType[_]] = depth match {
    case 1 => Gen.oneOf(dataTypeGen, pairTypeGen(dataTypeGen, 1), arrayTypeGen(dataTypeGen, 1))
    case _ =>
      Gen.oneOf(dataTypeGen, pairTypeGen(getFullTypeGen(depth - 1), 1), arrayTypeGen(getFullTypeGen(depth - 1), 1))
  }

  def getArrayGen(depth: Int) = arrayTypeGen(getFullTypeGen(depth - 1), 1)

  def getPairGen(depth: Int) = pairTypeGen(getFullTypeGen(depth - 1), 1)

  def optionTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[OptionType[_]] = {
    def optionTypeGenFinal(itemGen: Gen[RType[_]]): Gen[OptionType[_]] = {
      for {item <- itemGen } yield new OptionType(item)
    }
    depth match {
      case 1 =>
        optionTypeGenFinal(itemGen)
      case _ =>
        Gen.oneOf(
          optionTypeGenFinal(itemGen),
          optionTypeGenFinal(optionTypeGen(itemGen, depth - 1))
        )
    }
  }

  def collTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[CollType[_]] = {
    def collTypeGenFinal(itemGen: Gen[RType[_]]): Gen[CollType[_]] = {
      for { item <- itemGen } yield new CollType(item)
    }
    depth match {
      case 1 =>
        collTypeGenFinal(itemGen)
      case _ =>
        Gen.oneOf(
          collTypeGenFinal(itemGen),
          collTypeGenFinal(collTypeGen(itemGen, depth - 1))
        )
    }
  }

  def replCollTypeGen(itemGen: Gen[RType[_]], depth: Int): Gen[ReplCollType[_]] = {
    def replCollTypeGenFinal(itemGen: Gen[RType[_]]): Gen[ReplCollType[_]] = {
      for { item <- itemGen } yield new ReplCollType(item)
    }
    depth match {
      case 1 =>
        replCollTypeGenFinal(itemGen)
      case _ =>
        Gen.oneOf(
          replCollTypeGenFinal(itemGen),
          replCollTypeGenFinal(replCollTypeGen(itemGen, depth - 1))
        )
    }
  }

  def extendedTypeGen(depth: Int): Gen[RType[_]] = depth match {
    case 1 => Gen.oneOf(dataTypeGen, pairTypeGen(dataTypeGen, 1), optionTypeGen(dataTypeGen, 1),
      arrayTypeGen(dataTypeGen, 1), collTypeGen(dataTypeGen, 1), replCollTypeGen(dataTypeGen, 1))
    case _ =>
      Gen.oneOf(dataTypeGen, pairTypeGen(extendedTypeGen(depth - 1), 1), optionTypeGen(extendedTypeGen(depth - 1), 1),
        arrayTypeGen(extendedTypeGen(depth - 1), 1), collTypeGen(extendedTypeGen(depth - 1), 1),
        replCollTypeGen(extendedTypeGen(depth - 1), 1)
      )
  }
}
