package special

import scalan.RType
import special.collection.Coll
import scala.reflect.{ClassTag, classTag}

package collection {
  case class CollType[A](tA: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
  }
  case class ReplCollType[A](tA: RType[A]) extends RType[ReplColl[A]] {
    val classTag: ClassTag[ReplColl[A]] = ClassTag[ReplColl[A]](classOf[ReplColl[A]])
  }
}

package object collection {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)
  implicit def replCollRType[A](implicit tA: RType[A]): RType[ReplColl[A]] = ReplCollType[A](tA)

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])
}
