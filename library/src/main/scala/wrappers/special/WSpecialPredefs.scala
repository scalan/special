package wrappers.special {
  import scalan._

  import impl._

  import special.wrappers.WrappersModule

  import special.wrappers.SpecialPredefWrapSpec

  import scala.collection.mutable.WrappedArray

  trait WSpecialPredefs extends Base { self: WrappersModule =>
    import WOption._;
    import WSpecialPredef._;
    @External("SpecialPredef") trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      @External def optionGetOrElse[A](opt: Rep[WOption[A]], default: Rep[A]): Rep[A];
      @External def none[@Reified A](implicit emA: Elem[A]): Rep[WOption[A]];
      @External def some[A](x: Rep[A]): Rep[WOption[A]];
      @External def cast[@Reified T](v: Rep[Any])(implicit emT: Elem[T]): Rep[WOption[T]];
      @External def loopUntil[A](s1: Rep[A], isMatch: Rep[scala.Function1[A, Boolean]], step: Rep[scala.Function1[A, A]]): Rep[A]
    }
  }
}