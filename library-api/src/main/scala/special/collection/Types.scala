package special.collection

import scalan.RType

import scala.reflect.ClassTag

object Types {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)

  case class CollType[A](tA: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = scala.reflect.classTag[Coll[A]]
  }
}


