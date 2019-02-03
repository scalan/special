package scalan

import scala.reflect.ClassTag
import scalan.util.CollectionUtil
import scalan.util.ReflectionUtil.ClassOps

trait TypeDesc extends Serializable {
  def getName(f: TypeDesc => String): String
  lazy val name: String = getName(_.name)

  // <> to delimit because: [] is used inside name; {} looks bad with structs.
  override def toString = s"${getClass.safeSimpleName}<$name>"
}

trait RType[A] {
  def classTag: ClassTag[A]
  def name: String = this.toString
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
    case _ => GeneralType[A](ctA)
  }).asInstanceOf[RType[A]]

  type SomeType = RType[_]

  /** Type descriptor for types with limited type information like Any, AnyRef, Nothing.
    * It also used to wrap class tags which are GenericClassTag instances (also with limited information about type structure).
    * To describe structure of the type more precisely use concrete classes in the hierarchy of RType. */
  case class GeneralType[A](classTag: ClassTag[A]) extends RType[A] {
    override def name: String = classTag match {
      case ClassTag.AnyRef => "AnyRef"
      case ct => ct.toString()
    }
  }

  case class PrimitiveType[A](classTag: ClassTag[A]) extends RType[A] {
    override def name: String = classTag.toString()
  }

  val AnyType      : RType[Any]      = GeneralType[Any]     (ClassTag.Any)
  val AnyRefType   : RType[AnyRef]   = GeneralType[AnyRef]  (ClassTag.AnyRef)
  val NothingType  : RType[Nothing]  = GeneralType[Nothing] (ClassTag.Nothing)

  implicit val BooleanType : RType[Boolean]  = PrimitiveType[Boolean] (ClassTag.Boolean)
  implicit val ByteType    : RType[Byte]     = PrimitiveType[Byte]    (ClassTag.Byte)
  implicit val ShortType   : RType[Short]    = PrimitiveType[Short]   (ClassTag.Short)
  implicit val IntType     : RType[Int]      = PrimitiveType[Int]     (ClassTag.Int)
  implicit val LongType    : RType[Long]     = PrimitiveType[Long]    (ClassTag.Long)
  implicit val CharType    : RType[Char]     = PrimitiveType[Char]    (ClassTag.Char)
  implicit val FloatType   : RType[Float]    = PrimitiveType[Float]   (ClassTag.Float)
  implicit val DoubleType  : RType[Double]   = PrimitiveType[Double]  (ClassTag.Double)
  implicit val UnitType    : RType[Unit]     = PrimitiveType[Unit]    (ClassTag.Unit)

  implicit case object StringType extends RType[String] {
    override def classTag: ClassTag[String] = ClassTag[String](classOf[String])
    override def name: String = "String"
  }

  case object RTypeType extends RType[RType[_]] {
    val classTag: ClassTag[RType[_]] = ClassTag[RType[_]](classOf[RType[_]])
    override def name: String = s"RType[Any]"
  }
  implicit def rtypeRType[A]: RType[RType[A]] = asType[RType[A]](RTypeType)

  implicit def pairRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[(A,B)] = PairType(tA, tB)

  case class PairType[A,B](tFst: RType[A], tSnd: RType[B]) extends RType[(A,B)] {
    val classTag: ClassTag[(A, B)] = scala.reflect.classTag[(A,B)]
    override def name: String = s"(${tFst.name}, ${tSnd.name})"
  }
  implicit def extendPairType[A,B](pt: RType[(A,B)]): PairType[A,B] = pt.asInstanceOf[PairType[A,B]]

  implicit def eitherRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[Either[A,B]] = EitherType(tA, tB)

  case class EitherType[A,B](tA: RType[A], tB: RType[B]) extends RType[Either[A,B]] {
    val classTag: ClassTag[Either[A, B]] = scala.reflect.classTag[Either[A,B]]
    override def name: String = s"(${tA.name} | ${tB.name})"
  }

  implicit def funcRType[A,B](implicit tDom: RType[A], tRange: RType[B]): RType[A => B] = FuncType(tDom, tRange)

  case class FuncType[A,B](tDom: RType[A], tRange: RType[B]) extends RType[A => B] {
    val classTag: ClassTag[A => B] = scala.reflect.classTag[A => B]
    override def name: String = s"${tDom.name} => ${tRange.name}"
  }

  type StructData = Array[AnyRef]

  def structRType(names: Array[String], types: Array[SomeType]): RType[StructData] = StructType(names, types)

  case class StructType(fieldNames: Array[String], fieldTypes: Array[SomeType]) extends RType[StructData] {
    val classTag: ClassTag[StructData] = scala.reflect.classTag[StructData]

    override def hashCode(): Int = {
      var h = CollectionUtil.deepHashCode(fieldNames)
      h += h * 31 + CollectionUtil.deepHashCode(fieldTypes)
      h
    }
    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: StructType =>
        java.util.Arrays.equals(fieldNames.asInstanceOf[Array[AnyRef]], that.fieldNames.asInstanceOf[Array[AnyRef]]) &&
        java.util.Arrays.equals(fieldTypes.asInstanceOf[Array[AnyRef]], that.fieldTypes.asInstanceOf[Array[AnyRef]])
      case _ => false
    })
  }

  type TupleData = Array[AnyRef]

  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)

  case class TupleType(items: Array[SomeType]) extends RType[StructData] {
    val classTag: ClassTag[TupleData] = scala.reflect.classTag[TupleData]
    override def name: String = items.map(_.name).mkString("(", ", ", ")")

    override def hashCode(): Int = CollectionUtil.deepHashCode(items)
    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: TupleType => java.util.Arrays.equals(items.asInstanceOf[Array[AnyRef]], that.items.asInstanceOf[Array[AnyRef]])
      case _ => false
    })
  }

  implicit def arrayRType[A](implicit tA: RType[A]): RType[Array[A]] = ArrayType(tA)

  case class ArrayType[A](tA: RType[A]) extends RType[Array[A]] {
    val classTag: ClassTag[Array[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Array[A]]
    }
    override def name: String = s"Array[${tA.name}]"
  }

  implicit def optionRType[A](implicit tA: RType[A]): RType[Option[A]] = OptionType(tA)

  case class OptionType[A](tA: RType[A]) extends RType[Option[A]] {
    val classTag: ClassTag[Option[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Option[A]]
    }
    override def name: String = s"Option[${tA.name}]"
  }

  type ThunkData[A] = () => A

  implicit def thunkRType[A](implicit tA: RType[A]): RType[ThunkData[A]] = ThunkType(tA)

  case class ThunkType[A](tA: RType[A]) extends RType[ThunkData[A]] {
    val classTag: ClassTag[ThunkData[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[ThunkData[A]]
    }
    override def name: String = s"Thunk[${tA.name}]"
  }

  @inline def asType[T](t: RType[_]): RType[T] = t.asInstanceOf[RType[T]]
}
