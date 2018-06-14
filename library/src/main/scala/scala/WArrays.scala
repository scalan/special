package scala {
  import scalan._

  import impl._

  import scala.wrappers.WrappersModule

  trait WArrays extends Base { self: WrappersModule =>
    @External("Array") @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] {
      implicit def eT: Elem[T];
      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
      @External def length: Rep[Int];
      @External def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]];
      @External def foldLeft[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, T], B]]): Rep[B];
      @External def filter(p: Rep[scala.Function1[T, Boolean]]): Rep[WArray[T]];
      @External def forall(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
      @External def exists(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
      @External def foreach(f: Rep[scala.Function1[T, Unit]]): Rep[Unit];
      @External def apply(i: Rep[Int]): Rep[T]
    };
    trait WArrayCompanion {
      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
    }
  }
}