package scalan

import scala.collection.immutable.ListMap
import scala.reflect.{AnyValManifest, ClassTag}
import scalan.util.Variance
import scalan.util.ReflectionUtil.ClassOps

trait TypeDesc extends Serializable {
  def getName(f: TypeDesc => String): String
  lazy val name: String = getName(_.name)

  // <> to delimit because: [] is used inside name; {} looks bad with structs.
  override def toString = s"${getClass.safeSimpleName}<$name>"
}

trait RType[A] extends TypeDesc {
  def classTag: ClassTag[A]
  // classTag.runtimeClass is cheap, no reason to make it lazy
  @inline final def runtimeClass: Class[_] = classTag.runtimeClass
  def buildTypeArgs: ListMap[String, (TypeDesc, Variance)] = ListMap()
  lazy val typeArgs: ListMap[String, (TypeDesc, Variance)] = buildTypeArgs
  def typeArgsIterator = typeArgs.valuesIterator.map(_._1)

  override def getName(f: TypeDesc => String) = {
    import ClassTag._
    val className = try { classTag match {
      case _: AnyValManifest[_] | Any | AnyVal | AnyRef | Object | Nothing | Null => classTag.toString
      case objectTag =>
        val cl = objectTag.runtimeClass
        val name = cl.safeSimpleName
        name
    }}
    catch {
      case t: Throwable =>
        ???
    }
    if (typeArgs.isEmpty)
      className
    else {
      val typeArgString = typeArgsIterator.map(f).mkString(", ")
      s"$className[$typeArgString]"
    }
  }
}

object RType {
  def apply[A](implicit t: RType[A]): RType[A] = t

  def fromClassTag[A](ctA: ClassTag[A]): RType[A] = (ctA match {
    case ClassTag.Boolean => BooleanType
    case ClassTag.Byte => ByteType
    case ClassTag.Short => ShortType
    case ClassTag.Int => IntType
    case ClassTag.Long => LongType
    case ClassTag.Char => CharType
    case ClassTag.Float => FloatType
    case ClassTag.Double => DoubleType
    case ClassTag.Unit => UnitType
    case _ => ConcreteType[A](ctA)
  }).asInstanceOf[RType[A]]

  type SomeType = RType[_]

  case class ConcreteType[A](classTag: ClassTag[A]) extends RType[A]

  val AnyType      : RType[Any]      = ConcreteType[Any]     (ClassTag.Any)
  val AnyRefType   : RType[AnyRef]   = ConcreteType[AnyRef]  (ClassTag.AnyRef)
  val NothingType  : RType[Nothing]  = ConcreteType[Nothing] (ClassTag.Nothing)

  implicit val BooleanType : RType[Boolean]  = ConcreteType[Boolean] (ClassTag.Boolean)
  implicit val ByteType    : RType[Byte]     = ConcreteType[Byte]    (ClassTag.Byte)
  implicit val ShortType   : RType[Short]    = ConcreteType[Short]   (ClassTag.Short)
  implicit val IntType     : RType[Int]      = ConcreteType[Int]     (ClassTag.Int)
  implicit val LongType    : RType[Long]     = ConcreteType[Long]    (ClassTag.Long)
  implicit val CharType    : RType[Char]     = ConcreteType[Char]    (ClassTag.Char)
  implicit val FloatType   : RType[Float]    = ConcreteType[Float]   (ClassTag.Float)
  implicit val DoubleType  : RType[Double]   = ConcreteType[Double]  (ClassTag.Double)
  implicit val UnitType    : RType[Unit]     = ConcreteType[Unit]    (ClassTag.Unit)

  implicit case object StringType extends RType[String] {
    override def classTag: ClassTag[String] = ClassTag[String](classOf[String])
  }

  implicit def rtypeRType[A](implicit tA: RType[A]): RType[RType[A]] = RTypeType(tA)

  case class RTypeType[A](tA: RType[A]) extends RType[RType[A]] {
    val classTag: ClassTag[RType[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[RType[A]]
    }
  }

  implicit def pairRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[(A,B)] = PairType(tA, tB)

  case class PairType[A,B](tA: RType[A], tB: RType[B]) extends RType[(A,B)] {
    val classTag: ClassTag[(A, B)] = scala.reflect.classTag[(A,B)]
  }

  implicit def eitherRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[Either[A,B]] = EitherType(tA, tB)

  case class EitherType[A,B](tA: RType[A], tB: RType[B]) extends RType[Either[A,B]] {
    val classTag: ClassTag[Either[A, B]] = scala.reflect.classTag[Either[A,B]]
  }

  implicit def funcRType[A,B](implicit tDom: RType[A], tRange: RType[B]): RType[A => B] = FuncType(tDom, tRange)

  case class FuncType[A,B](tDom: RType[A], tRange: RType[B]) extends RType[A => B] {
    val classTag: ClassTag[A => B] = scala.reflect.classTag[A => B]
  }

  type StructData = Array[AnyRef]

  implicit def structRType(names: Array[String], types: Array[SomeType]): RType[StructData] = StructType(names, types)

  case class StructType(fieldNames: Array[String], fieldTypes: Array[SomeType]) extends RType[StructData] {
    val classTag: ClassTag[StructData] = scala.reflect.classTag[StructData]
  }

  implicit def arrayRType[A](implicit tA: RType[A]): RType[Array[A]] = ArrayType(tA)

  case class ArrayType[A](tA: RType[A]) extends RType[Array[A]] {
    val classTag: ClassTag[Array[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Array[A]]
    }
  }

  implicit def optionRType[A](implicit tA: RType[A]): RType[Option[A]] = OptionType(tA)

  case class OptionType[A](tA: RType[A]) extends RType[Option[A]] {
    val classTag: ClassTag[Option[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Option[A]]
    }
  }

  type ThunkData[A] = () => A

  implicit def thunkRType[A](implicit tA: RType[A]): RType[ThunkData[A]] = ThunkType(tA)

  case class ThunkType[A](tA: RType[A]) extends RType[ThunkData[A]] {
    val classTag: ClassTag[ThunkData[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[ThunkData[A]]
    }
  }

}
