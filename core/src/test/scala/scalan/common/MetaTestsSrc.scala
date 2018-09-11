package scalan.common

trait MetaTest[T] {
  def test: MetaTest[T]
  def give: T
  def size: Int
}

class MT0(val size: Int) extends MetaTest[Unit] {
  def test: MetaTest[Unit] = ???
  def give: Unit = ???
}

class MT1[T](val data: T, val size: Int) extends MetaTest[T] {
  def test: MetaTest[T] = ???
  def give: T = ???
}

class MT2[A, B](val indices: A, val values: B, val size: Int) extends MetaTest[(A, B)] {
  def test: MetaTest[(A, B)] = ???
  def give: (A, B) = ???
}
