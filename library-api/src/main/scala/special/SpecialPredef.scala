package special

import scala.reflect.ClassTag

object SpecialPredef {
  def loopUntil[A](s1: A, isMatch: A => Boolean, step: A => A): A = {
    var res = s1
    while (!isMatch(res))
      res = step(res)
    res
  }

  def cast[T:ClassTag](v: Any): Option[T] = v match { case _: T => Some(v.asInstanceOf[T]) case _ =>  None }

  def eitherMap[A,B,C,D](e: Either[A,B], fa: A => C, fb: B => D): Either[C,D] = e.right.map(fb).left.map(fa)

  def some[A](x: A): Option[A] = Some(x)

  def none[A:ClassTag]: Option[A] = Option.empty[A]

  def left[A,B](a: A): Either[A,B] = Left(a)
  
  def right[A,B](b: B): Either[A,B] = Right(b)

  def optionGetOrElse[A](opt: Option[A], default: A): A = opt.getOrElse(default)

  def rewritableMethod =
    sys.error(s"Shouldn't be called, instead it should be either handled in rewrite rule, or overridden in derived class.")
}
