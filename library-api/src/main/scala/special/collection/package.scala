package special

import scalan.RType
import special.collection.Coll
import scala.reflect.ClassTag

package collection {
  case class CollType[A](tA: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
  }
}

package object collection {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)


}
