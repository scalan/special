package special.collection

import scala.reflect.runtime.universe
import scalan.meta.RType

object Types {
  implicit def colRType[A](implicit tA: RType[A]): RType[Coll[A]] = ColRType[A](tA)

  case class ColRType[A](tA: RType[A]) extends RType[Coll[A]] {
    def tag: universe.WeakTypeTag[Coll[A]] = universe.weakTypeTag[Coll[A]]
  }
}


