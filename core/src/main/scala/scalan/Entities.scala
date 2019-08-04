package scalan

import java.util.Objects

import scala.annotation.tailrec
import scala.language.higherKinds
import scalan.util.ReflectionUtil

trait Entities extends TypeDescs { self: Scalan =>
  import BaseConverter._
  
  abstract class EntityElem[A] extends Elem[A] with scala.Equals {
    def parent: Option[Elem[_]] = None
    def entityName: String = {
      val n = this.getClass.getSimpleName.stripSuffix("Elem")
      n
    }
    def convert(x: Rep[Def[_]]): Rep[A] = !!!("should not be called")
    def canEqual(other: Any) = other.isInstanceOf[EntityElem[_]]

    override def equals(other: Any) = other match {
      case other: EntityElem[_] =>
        this.eq(other) ||
          (other.canEqual(this) &&
            this.getClass == other.getClass &&
            this.typeArgsDescs == other.typeArgsDescs)
      case _ => false
    }

    override def hashCode = Objects.hash(getClass, typeArgsDescs)
  }

  abstract class EntityElem1[A, To, C[_]](val eItem: Elem[A], val cont: Cont[C])
    extends EntityElem[To] {
    override def getName(f: TypeDesc => String) = {
      s"${f(cont)}[${f(eItem)}]"
    }
    override def canEqual(other: Any) = other match {
      case _: EntityElem1[_, _, _] => true
      case _ => false
    }
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: EntityElem1[_,_,_] =>
        other.canEqual(this) && cont == other.cont && eItem == other.eItem
      case _ => false
    })
    override def hashCode = eItem.hashCode * 41 + cont.hashCode
  }
  trait ConcreteElem[TData, TClass] extends EntityElem[TClass] with ViewElem[TData, TClass] { eClass =>
    def getConverterFrom[E](eEntity: EntityElem[E]): Option[Conv[E, TClass]] = {
      try {
        val convFun: Rep[E => TClass] =
          fun({ x: Rep[E] => eClass.convert(asRep[Def[_]](x))})(Lazy(eEntity))
        Some(RBaseConverter(convFun))
      }
      catch {
        case e: RuntimeException => None
      }
    }
  }
  trait ConcreteElem1[A, TData, TClass, C[_]]
    extends EntityElem1[A, TClass, C]
       with ViewElem1[A, TData, TClass, C] { eClass =>
  }

  implicit class EntityElemExtensions[A <: Def[_]](e: Elem[A]) {
    def asEntityElem = e.asInstanceOf[EntityElem[A]]
  }

  def isConcreteElem(e: TypeDesc): Boolean = e match {
    case _: BaseElem[_] =>
      true
    case e: EntityElem[_] if !isConcreteModuloTypeArgs(e) =>
      false
    case e: Elem[_] =>
      e.typeArgsDescs.forall(isConcreteElem)
    case _: Cont[_] => true
  }

  protected def isConcreteModuloTypeArgs(e: EntityElem[_]) = e match {
    case _: ViewElem[_, _] => true
    case _ => false
  }

  implicit class ElemOpsForEntities[T](e: Elem[T]) {
    def isConcrete = isConcreteElem(e)
  }
  trait CompanionElem[T] extends Elem[T] { _: scala.Equals =>
    override def buildTypeArgs = TypeArgs()
  }

  trait TypeFamily1[F[_]]
  trait TypeFamily2[F[_, _]]
  trait TypeFamily3[F[_, _, _]]

  trait ConcreteClass0[C]
  trait ConcreteClass1[C[_]]
  trait ConcreteClass2[C[_, _]]
  trait ConcreteClass3[T[_, _, _]]
  trait ConcreteClass4[T[_, _, _, _]]

  implicit class RepDefViewOps[T <: Def[_]](x: Rep[T]) {
    def convertTo[R <: Def[_]](implicit eR: Elem[R]): Rep[R] =
      eR match {
        case entE: EntityElem[R] @unchecked => entE.convert(x)
        case _ => !!!(s"Cannot convert $x to a value of type ${eR.name}: EntityElem expected but ${eR.getClass.getSimpleName} found", x)
      }
  }
}
