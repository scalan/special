package special.collection {
  import scalan._

  trait Colls extends Base { self: Library =>
    import ColBuilder._;
    import Coll._;
    import Monoid._;
    import PairColl._;
    import WArray._;
    @ContainerType @FunctorType @Liftable trait Coll[A] extends Def[Coll[A]] {
      implicit def eA: Elem[A];
      def builder: Rep[ColBuilder];
      def arr: Rep[WArray[A]];
      def length: Rep[Int];
      def apply(i: Rep[Int]): Rep[A];
      def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A];
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Coll[B]];
      def zip[B](ys: Rep[Coll[B]]): Rep[PairColl[A, B]];
      def foreach(f: Rep[scala.Function1[A, Unit]]): Rep[Unit];
      def exists(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean];
      def forall(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean];
      def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[Coll[A]];
      def where(p: Rep[scala.Function1[A, Boolean]]): Rep[Coll[A]] = this.filter(p);
      def fold[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, A], B]]): Rep[B];
      def sum(m: Rep[Monoid[A]]): Rep[A];
      def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]];
      def append(other: Rep[Coll[A]]): Rep[Coll[A]]
    };
    trait PairColl[L, R] extends Coll[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def ls: Rep[Coll[L]];
      def rs: Rep[Coll[R]]
    };
    @Liftable trait ReplColl[A] extends Coll[A] {
      implicit def eA: Elem[A];
      def value: Rep[A];
      def length: Rep[Int];
      def append(other: Rep[Coll[A]]): Rep[Coll[A]]
    };
    @Liftable trait ColBuilder extends Def[ColBuilder] {
      def pairCol[A, B](as: Rep[Coll[A]], bs: Rep[Coll[B]]): Rep[PairColl[A, B]];
      @Reified(value = "T") def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]];
      @NeverInline def unzip[A, B](xs: Rep[Coll[scala.Tuple2[A, B]]]): Rep[scala.Tuple2[Coll[A], Coll[B]]] = delayInvoke;
      def xor(left: Rep[Coll[Byte]], right: Rep[Coll[Byte]]): Rep[Coll[Byte]];
      def fromArray[T](arr: Rep[WArray[T]]): Rep[Coll[T]];
      def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Coll[T]]
    };
    trait CollCompanion;
    trait PairCollCompanion;
    trait ReplCollCompanion;
    trait ColBuilderCompanion
  }
}