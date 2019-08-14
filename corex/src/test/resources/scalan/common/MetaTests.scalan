package scalan.common

import scalan._

trait MetaTests { self: MetaTestsModule =>
  type RMetaTest[T] = Ref[MetaTest[T]]
  @Liftable
  @Convertible
  @WithMethodCallRecognizers
  trait MetaTest[T] extends Def[MetaTest[T]] { self =>
    def test: RMetaTest[T]
    def give: Ref[T]
    def size: Ref[Int]
    implicit def eT: Elem[T]
    @Reified(value = "B") def fromItems[B](items: Ref[B]*)(implicit cB: Elem[B]): Ref[MetaTest[B]]
  }
  trait MetaTestCompanion

  @WithMethodCallRecognizers
  abstract class MT0(val size: Ref[Int]) extends MetaTest[Unit] {

    def test: RMetaTest[Unit] = ???
    def give: Ref[Unit] = ???
    def eT = UnitElement

    def fromItems[B](items: Ref[B]*)(implicit cB: Elem[B]): Ref[MetaTest[B]] = ???
  }
  trait MT0Companion

  @WithMethodCallRecognizers
  abstract class MT1[T](val data: Ref[T], val size: Ref[Int]) extends MetaTest[T] {
    def test: RMetaTest[T] = ???
    def give: Ref[T] = ???
    def fromItems[B](items: Ref[B]*)(implicit cB: Elem[B]): Ref[MetaTest[B]] = ???
  }

  @Liftable
  @WithMethodCallRecognizers
  trait MetaPair[A,B] extends MetaTest[(A,B)] {
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    def indices: Ref[A]
    def values: Ref[B]
    def give: Ref[(A, B)]
  }

  @WithMethodCallRecognizers
  @Isospec
  abstract class MT2[A, B](val indices: Ref[A], val values: Ref[B], val size: Ref[Int])
    extends MetaPair[A, B] {
    implicit def eA: Elem[A]; implicit def eB: Elem[B]
    def test: RMetaTest[(A, B)] = ???
    def give: Ref[(A, B)] = ???
    def fromItems[C](items: Ref[C]*)(implicit cB: Elem[C]): Ref[MetaTest[C]] = ???
  }
}

trait MetaTestsModule extends impl.MetaTestsDefs
