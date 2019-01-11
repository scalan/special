package wrappers.scala {
  import scalan._

  import impl._

  import special.wrappers.WrappersModule

  import special.wrappers.ArrayWrapSpec

  trait WArrays extends Base { self: WrappersModule =>
    import WArray._;
    @External("Array") @ContainerType @FunctorType @Liftable trait WArray[T] extends Def[WArray[T]] {
      implicit def eT: Elem[T];
      @External def apply(i: Rep[Int]): Rep[T];
      @External def foreach(f: Rep[scala.Function1[T, Unit]]): Rep[Unit];
      @External def exists(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
      @External def forall(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
      @External def filter(p: Rep[scala.Function1[T, Boolean]]): Rep[WArray[T]];
      @External def foldLeft[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, T], B]]): Rep[B];
      @External def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]];
      @External def length: Rep[Int];
      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]]
    };
    trait WArrayCompanion {
      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
    }
  }
}