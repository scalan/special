package special.collection

import scala.reflect.runtime.universe
import scalan.meta.RType

object Types {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollRType[A](tA)

  case class CollRType[A](tA: RType[A]) extends RType[Coll[A]] {
    def tag: universe.WeakTypeTag[Coll[A]] = universe.weakTypeTag[Coll[A]]
  }
}


