package scalan.meta

import scala.collection.immutable.ListMap
import scala.reflect.{ClassTag, AnyValManifest}
import scala.reflect.runtime.universe.WeakTypeTag
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
  implicit def weakTypeTagToRType[A](implicit tag: WeakTypeTag[A]): RType[A] = WeakRType(tag)
  implicit def rtypeToClassTag[A](implicit t: RType[A]): ClassTag[A] = t.classTag

  case class WeakRType[A](tag: WeakTypeTag[A]) extends RType[A] {
  }
}
