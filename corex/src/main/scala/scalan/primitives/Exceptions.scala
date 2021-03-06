package scalan.primitives

import scalan.Scalan

trait Exceptions { self: Scalan =>
  case class ThrowException(msg: Ref[String]) extends BaseDef[Unit] {
    override def transform(t: Transformer) = ThrowException(t(msg))
  }
  def THROW(msg: Ref[String]): Ref[Unit] = ThrowException(msg)
}
