package scalan.collection {
  import scalan._

  trait Monoids extends Base { self: Library =>
    trait Monoid[T] extends Def[Monoid[T]] {
      implicit def eT: Elem[T];
      def zero: Rep[T];
      def plus(x: Rep[T], y: Rep[T]): Rep[T];
      def power(x: Rep[T], n: Rep[Int]): Rep[T]
    };
    trait MonoidBuilder extends Def[MonoidBuilder] {
      def intPlusMonoid: Rep[Monoid[Int]];
      def longPlusMonoid: Rep[Monoid[Long]]
    };
    trait MonoidCompanion;
    trait MonoidBuilderCompanion
  }
}