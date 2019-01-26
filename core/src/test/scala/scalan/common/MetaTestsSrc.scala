package scalan.common

import scalan.{Liftable, RType}

@Liftable
trait MetaTest[T] {
  def test: MetaTest[T]
  def give: T
  def size: Int
}
object MetaTest {
  implicit def metaTestRType[T:RType]: RType[MetaTest[T]] = RType.fromClassTag(scala.reflect.classTag[MetaTest[T]])
}

class MT0(val size: Int) extends MetaTest[Unit] {
  def test: MetaTest[Unit] = ???
  def give: Unit = ???
}

class MT1[T](val data: T, val size: Int) extends MetaTest[T] {
  def test: MetaTest[T] = ???
  def give: T = ???
}

@Liftable
trait MetaPair[A,B] extends MetaTest[(A,B)] {
  def indices: A
  def values: B
  def give: (A, B)
}
object MetaPair {
  implicit def metaPairRType[A:RType, B:RType]: RType[MetaPair[A,B]] = RType.fromClassTag(scala.reflect.classTag[MetaPair[A,B]])
}

class MT2[A, B](val indices: A, val values: B, val size: Int) extends MetaPair[A, B] {
  def test: MetaTest[(A, B)] = ???
  def give: (A, B) = ???
}
