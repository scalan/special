package special.collection

import scala.reflect.runtime.universe
import scalan.meta.RType

object Types {
  implicit def colRType[A](implicit tA: RType[A]): RType[Col[A]] = ColRType[A](tA)

  case class ColRType[A](tA: RType[A]) extends RType[Col[A]] {
    def tag: universe.WeakTypeTag[Col[A]] = universe.weakTypeTag[Col[A]]
  }
}


