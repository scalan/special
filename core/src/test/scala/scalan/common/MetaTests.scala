package scalan.common

import scalan._

trait MetaTests { self: MetaTestsModule =>
  type RMetaTest[T] = Rep[MetaTest[T]]
  @scalan.Liftable
  trait MetaTest[T] extends Def[MetaTest[T]] { self =>
    def test: RMetaTest[T]
    def give: Rep[T]
    def size: Rep[Int]
    implicit def eT: Elem[T]
    @Reified(value = "B") def fromItems[B](items: Rep[B]*)(implicit cB: Elem[B]): Rep[MetaTest[B]]
  }
  trait MetaTestCompanion

  abstract class MT0(val size: Rep[Int]) extends MetaTest[Unit] {

    def test: RMetaTest[Unit] = ???
    def give: Rep[Unit] = ???
    def eT = UnitElement

    def fromItems[B](items: Rep[B]*)(implicit cB: Elem[B]): Rep[MetaTest[B]] = ???
  }
  trait MT0Companion

  abstract class MT1[T](val data: Rep[T], val size: Rep[Int]) extends MetaTest[T] {
    def test: RMetaTest[T] = ???
    def give: Rep[T] = ???
    def fromItems[B](items: Rep[B]*)(implicit cB: Elem[B]): Rep[MetaTest[B]] = ???
  }

  @scalan.Liftable
  trait MetaPair[A,B] extends MetaTest[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def indices: Rep[A]
    def values: Rep[B]
    def give: Rep[(A, B)]
  }

  abstract class MT2[A, B](val indices: Rep[A], val values: Rep[B], val size: Rep[Int])
    extends MetaPair[A, B] {
    implicit def eA: Elem[A]; implicit def eB: Elem[B]
    def test: RMetaTest[(A, B)] = ???
    def give: Rep[(A, B)] = ???
    def fromItems[C](items: Rep[C]*)(implicit cB: Elem[C]): Rep[MetaTest[C]] = ???
  }
}

trait MetaTestsModule extends impl.MetaTestsDefs
