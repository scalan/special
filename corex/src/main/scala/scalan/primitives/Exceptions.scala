package scalan.primitives

import scalan.Scalan

trait Exceptions { self: Scalan =>
  case class ThrowException(msg: Rep[String]) extends BaseDef[Unit] {
    override def transform(t: Transformer) = ThrowException(t(msg))
  }
  def THROW(msg: Rep[String]): Rep[Unit] = ThrowException(msg)
}
