package scalan

import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen}

class GenConfiguration(val maxArrayLength: Int = 5) {}

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

  def primitiveValueGen[T: RType](t: PrimitiveType[T]): Gen[T] = t match {
    case ByteType => choose[Byte](Byte.MinValue, Byte.MaxValue).asInstanceOf[Gen[T]]
    case ShortType => choose[Short](Short.MinValue, Short.MaxValue).asInstanceOf[Gen[T]]
    case IntType => choose[Int](Int.MinValue, Int.MaxValue).asInstanceOf[Gen[T]]
    case CharType => choose[Char](Char.MinValue, Char.MaxValue).asInstanceOf[Gen[T]]
    case LongType => choose[Long](Long.MinValue, Long.MaxValue).asInstanceOf[Gen[T]]
    case FloatType => choose[Float](Float.MinValue, Float.MaxValue).asInstanceOf[Gen[T]]
    case DoubleType => choose[Double](Double.MinValue, Double.MaxValue).asInstanceOf[Gen[T]]
    case BooleanType => Gen.oneOf(true, false).asInstanceOf[Gen[T]]
    case _ => throw new RuntimeException(s"Can't interpret ${t} as non-unit primitive type.")
  }

  val builder: CollBuilder = new CollOverArrayBuilder

  def getArrayGen[T](valGen: Gen[T], count: Int)(implicit t: RType[T]): Gen[Array[T]] = {
    containerOfN[Array, T](count, valGen)
  }

  def getCollOverArrayGen[T: RType](valGen: Gen[T], count: Int): Gen[Coll[T]] = {
    getArrayGen(valGen, count).map(builder.fromArray(_))
  }

  def getCollReplGen[T: RType](valGen: Gen[T], count: Int): Gen[Coll[T]] = {
    for { l <- choose(0, count); v <- valGen } yield new CReplColl(v, l)
  }

  def getCollViewGen[A: RType](valGen: Gen[Coll[A]]): Gen[Coll[A]] = {
    valGen.map(builder.makeView(_, identity[A]))
  }

  def getCollViewGen[A, B: RType](valGen: Gen[Coll[A]], f: A => B): Gen[Coll[B]] = {
    valGen.map(builder.makeView(_, f))
  }

  def rtypeValueGen[T](conf: GenConfiguration)(implicit t: RType[T]): Gen[T] = t match {
    case prim: PrimitiveType[a] =>
      primitiveValueGen(prim)(prim)
    case arrayType: ArrayType[a] =>
      getArrayGen(rtypeValueGen(conf)(arrayType.tA).asInstanceOf[Gen[a]], conf.maxArrayLength)(arrayType.tA)
    case pairType: PairType[a, b] =>
      for { left <- rtypeValueGen(conf)(pairType.tFst); right <- rtypeValueGen(conf)(pairType.tSnd) }
        yield (left.asInstanceOf[a], right.asInstanceOf[b])
    case StringType =>
      Gen.asciiPrintableStr
    case collType: CollType[a] => collType.tItem match {
      case pairType: PairType[fst, snd] =>
        val tA = pairType.tFst
        val tB = pairType.tSnd
        for {
          left <- getCollOverArrayGen(rtypeValueGen(conf)(tA), conf.maxArrayLength)(tA);
          right <- getCollOverArrayGen(rtypeValueGen(conf)(tB), conf.maxArrayLength)(tB)
        } yield new PairOfCols(left, right)
      case _ => getCollOverArrayGen(rtypeValueGen(conf)(collType.tItem), conf.maxArrayLength)(collType.tItem)
    }
    case replCollType: ReplCollType[a] =>
      getCollReplGen(rtypeValueGen(conf)(replCollType.tItem), conf.maxArrayLength)(replCollType.tItem).asInstanceOf[Gen[T]]
    case optionType: OptionType[a] =>
      Gen.option(rtypeValueGen(conf)(optionType.tA))
    case _ => throw new RuntimeException(s"Can't create generator for ${t}: this type is still not supported.")
  }
}
