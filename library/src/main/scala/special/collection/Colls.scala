package special.collection {
  import scalan._

  trait Colls extends Base { self: Library =>
    import Coll._;
    import CollBuilder._;
    import Monoid._;
    import MonoidBuilder._;
    import PairColl._;
    import WOption._;
    @ContainerType @FunctorType @Liftable @WithMethodCallRecognizers trait Coll[A] extends Def[Coll[A]] {
      implicit def eA: Elem[A];
      def builder: Rep[CollBuilder];
      def length: Rep[Int];
      def size: Rep[Int] = this.length;
      def isEmpty: Rep[Boolean];
      def nonEmpty: Rep[Boolean];
      def apply(i: Rep[Int]): Rep[A];
      def isDefinedAt(idx: Rep[Int]): Rep[Boolean];
      def getOrElse(index: Rep[Int], default: Rep[A]): Rep[A];
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Coll[B]];
      def zip[B](ys: Rep[Coll[B]]): Rep[Coll[scala.Tuple2[A, B]]];
      def exists(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean];
      def forall(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean];
      def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[Coll[A]];
      def foldLeft[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, A], B]]): Rep[B];
      def indices: Rep[Coll[Int]];
      def flatMap[B](f: Rep[scala.Function1[A, Coll[B]]]): Rep[Coll[B]];
      def segmentLength(p: Rep[scala.Function1[A, Boolean]], from: Rep[Int]): Rep[Int];
      @NeverInline def find(p: Rep[scala.Function1[A, Boolean]]): Rep[WOption[A]] = delayInvoke;
      def indexWhere(p: Rep[scala.Function1[A, Boolean]], from: Rep[Int]): Rep[Int];
      @NeverInline def indexOf(elem: Rep[A], from: Rep[Int]): Rep[Int] = delayInvoke;
      def lastIndexWhere(p: Rep[scala.Function1[A, Boolean]], end: Rep[Int]): Rep[Int];
      def take(n: Rep[Int]): Rep[Coll[A]];
      def partition(pred: Rep[scala.Function1[A, Boolean]]): Rep[scala.Tuple2[Coll[A], Coll[A]]];
      def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]];
      def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]];
      def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]];
      def mapReduce[K, V](m: Rep[scala.Function1[A, scala.Tuple2[K, V]]], r: Rep[scala.Function1[scala.Tuple2[V, V], V]]): Rep[Coll[scala.Tuple2[K, V]]];
      @NeverInline def groupBy[K](key: Rep[scala.Function1[A, K]]): Rep[Coll[scala.Tuple2[K, Coll[A]]]] = delayInvoke;
      @NeverInline def groupByProjecting[K, V](key: Rep[scala.Function1[A, K]], proj: Rep[scala.Function1[A, V]]): Rep[Coll[scala.Tuple2[K, Coll[V]]]] = delayInvoke;
      def unionSet(that: Rep[Coll[A]]): Rep[Coll[A]];
      @NeverInline def diff(that: Rep[Coll[A]]): Rep[Coll[A]] = delayInvoke;
      @NeverInline def intersect(that: Rep[Coll[A]]): Rep[Coll[A]] = delayInvoke;
      def sum(m: Rep[Monoid[A]]): Rep[A];
      def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]];
      def append(other: Rep[Coll[A]]): Rep[Coll[A]];
      def reverse: Rep[Coll[A]]
    };
    @WithMethodCallRecognizers trait PairColl[L, R] extends Coll[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def ls: Rep[Coll[L]];
      def rs: Rep[Coll[R]];
      def mapFirst[T1](f: Rep[scala.Function1[L, T1]]): Rep[Coll[scala.Tuple2[T1, R]]];
      def mapSecond[T1](f: Rep[scala.Function1[R, T1]]): Rep[Coll[scala.Tuple2[L, T1]]]
    };
    @Liftable @WithMethodCallRecognizers trait ReplColl[A] extends Coll[A] {
      implicit def eA: Elem[A];
      def value: Rep[A];
      def length: Rep[Int];
      def append(other: Rep[Coll[A]]): Rep[Coll[A]]
    };
    @Liftable @WithMethodCallRecognizers trait CollBuilder extends Def[CollBuilder] {
      def Monoids: Rep[MonoidBuilder];
      def pairColl[A, B](as: Rep[Coll[A]], bs: Rep[Coll[B]]): Rep[PairColl[A, B]];
      @Reified(value = "T") def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]];
      def unzip[A, B](xs: Rep[Coll[scala.Tuple2[A, B]]]): Rep[scala.Tuple2[Coll[A], Coll[B]]];
      def xor(left: Rep[Coll[Byte]], right: Rep[Coll[Byte]]): Rep[Coll[Byte]];
      def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Coll[T]];
      def emptyColl[T](implicit tT: Elem[T]): Rep[Coll[T]];
      def outerJoin[K, L, R, O](left: Rep[Coll[scala.Tuple2[K, L]]], right: Rep[Coll[scala.Tuple2[K, R]]])(l: Rep[scala.Function1[scala.Tuple2[K, L], O]], r: Rep[scala.Function1[scala.Tuple2[K, R], O]], inner: Rep[scala.Function1[scala.Tuple2[K, scala.Tuple2[L, R]], O]]): Rep[Coll[scala.Tuple2[K, O]]];
      def flattenColl[A](coll: Rep[Coll[Coll[A]]]): Rep[Coll[A]]
    };
    trait CollCompanion;
    trait PairCollCompanion;
    trait ReplCollCompanion;
    trait CollBuilderCompanion
  }
}