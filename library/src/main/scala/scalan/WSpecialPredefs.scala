package scalan {
  import scalan._

  import impl._

  import scala.wrappers.WrappersModule

  trait WSpecialPredefs extends Base { self: WrappersModule =>
    @External("SpecialPredef") trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      @External def optionGetOrElse[A](opt: Rep[WOption[A]], default: Rep[A]): Rep[A];
      @External def right[A, B](b: Rep[B])(implicit emA: Elem[A]): Rep[WEither[A, B]];
      @External def left[A, B](a: Rep[A])(implicit emB: Elem[B]): Rep[WEither[A, B]];
      @External def none[@Reified A](implicit emA: Elem[A]): Rep[WOption[A]];
      @External def some[A](x: Rep[A]): Rep[WOption[A]];
      @External def eitherMap[A, B, C, D](e: Rep[WEither[A, B]], fa: Rep[scala.Function1[A, C]], fb: Rep[scala.Function1[B, D]]): Rep[WEither[C, D]];
      @External def cast[@Reified T](v: Rep[Any])(implicit emT: Elem[T]): Rep[WOption[T]];
      @External def loopUntil[A](s1: Rep[A], isMatch: Rep[scala.Function1[A, Boolean]], step: Rep[scala.Function1[A, A]]): Rep[A]
    }
  }
}