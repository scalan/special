package scalan.common

import scalan._

trait Kinds extends Base { self: KindsModule =>
  import Kind._; import Bind._; import Return._;
  type RKind[F[_],A] = Ref[Kind[F,A]]

  @Convertible
  @WithMethodCallRecognizers
  trait Kind[F[_], A] extends Def[Kind[F,A]] {
    implicit def eA: Elem[A]
    implicit def cF: Cont[F]

    def flatMap[B](f: Ref[A] => Ref[Kind[F,B]]): Ref[Kind[F,B]] = RBind(self, fun(f))

    def mapBy[B](f: Ref[A => B]): Ref[Kind[F,B]] =
      flatMap(a => RReturn(f(a)))
  }
  trait KindCompanion

  @WithMethodCallRecognizers
  @Isospec
  abstract class Return[F[_],A]
        (val a: Ref[A])
        (implicit val cF: Cont[F]) extends Kind[F,A]
  {
    override def flatMap[B](f: Ref[A] => Ref[Kind[F,B]]): Ref[Kind[F,B]] = f(a)
  }
  trait ReturnCompanion

  @WithMethodCallRecognizers
  @Isospec
  abstract class Bind[F[_],S,B]
        (val a: Ref[Kind[F, S]], val f: Ref[S => Kind[F,B]]) extends Kind[F,B] {
    override def flatMap[R](f1: Ref[B] => Ref[Kind[F,R]]): Ref[Kind[F,R]] = {
      a.flatMap((s: Ref[S]) => f(s).flatMap(f1))
    }
  }
  trait BindCompanion
}
