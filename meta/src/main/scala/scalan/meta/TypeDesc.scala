package scalan.meta

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe
import scala.reflect.{AnyValManifest, ClassTag}
import scala.reflect.runtime.universe.{WeakTypeTag, TypeTag}
import scalan.util.{Variance, ReflectionUtil}
import scalan.util.ReflectionUtil.ClassOps

trait TypeDesc extends Serializable {
  def getName(f: TypeDesc => String): String
  lazy val name: String = getName(_.name)

  // <> to delimit because: [] is used inside name; {} looks bad with structs.
  override def toString = s"${getClass.safeSimpleName}<$name>"
}

trait RType[A] extends TypeDesc {
  def tag: WeakTypeTag[A]
  final lazy val classTag: ClassTag[A] = ReflectionUtil.typeTagToClassTag(tag)
  // classTag.runtimeClass is cheap, no reason to make it lazy
  final def runtimeClass: Class[_] = classTag.runtimeClass
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
  implicit def rtypeToClassTag[A](implicit t: RType[A]): ClassTag[A] = t.classTag

  case class ConcreteRType[A](tag: WeakTypeTag[A]) extends RType[A]

  implicit val BooleanType : RType[Boolean]  = ConcreteRType[Boolean] (universe.weakTypeTag[Boolean])
  implicit val ByteType    : RType[Byte]     = ConcreteRType[Byte]    (universe.weakTypeTag[Byte])
  implicit val ShortType   : RType[Short]    = ConcreteRType[Short]   (universe.weakTypeTag[Short])
  implicit val IntType     : RType[Int]      = ConcreteRType[Int]     (universe.weakTypeTag[Int])
  implicit val LongType    : RType[Long]     = ConcreteRType[Long]    (universe.weakTypeTag[Long])
  implicit val StringType  : RType[String]   = ConcreteRType[String]  (universe.weakTypeTag[String])

  implicit def pairRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[(A,B)] = PairRType(tA, tB)

  case class PairRType[A,B](tA: RType[A], tB: RType[B]) extends RType[(A,B)] {
    def tag: universe.WeakTypeTag[(A, B)] = universe.weakTypeTag[(A,B)]
  }

  object syntax {
    implicit def rtypeFromTypeTag[A](implicit tag: TypeTag[A]): RType[A] = RType[A]
  }
}
