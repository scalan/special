package special.collection

@scalan.Liftable
trait Size[Val] {
  def dataSize: Long
}

@scalan.Liftable
trait SizePrim[Val] extends Size[Val] {
  def dataSize: Long
}

@scalan.Liftable
trait SizePair[L,R] extends Size[(L,R)] {
  def l: Size[L]
  def r: Size[R]
}

@scalan.Liftable
trait SizeColl[Item] extends Size[Coll[Item]] {
  def sizes: Coll[Size[Item]]
}

@scalan.Liftable
trait SizeFunc[Env, Arg, Res] extends Size[Arg => Res] {
  def sizeEnv: Size[Env]
}

@scalan.Liftable
trait SizeOption[T] extends Size[Option[T]] {
  def sizeOpt: Option[Size[T]]
}

