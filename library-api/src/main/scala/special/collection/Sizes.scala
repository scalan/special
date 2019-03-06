package special.collection

trait Size[Val] {
  def dataSize: Long
}

trait SizePrim[Val] extends Size[Val] {
  def dataSize: Long
}

trait SizePair[L,R] extends Size[(L,R)] {
  def l: Size[L]
  def r: Size[R]
}

trait SizeColl[Item] extends Size[Coll[Item]] {
  def sizes: Coll[Size[Item]]
}

trait SizeFunc[Env, Arg, Res] extends Size[Arg => Res] {
  def sizeEnv: Size[Env]
}

trait SizeOption[T] extends Size[Option[T]] {
  def sizeOpt: Option[Size[T]]
}

