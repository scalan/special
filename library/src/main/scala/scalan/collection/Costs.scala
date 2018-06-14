package scalan.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    trait Costed[T] extends Def[Costed[T]] {
      implicit def eT: Elem[T];
      def builder: Rep[CostedBuilder];
      def value: Rep[T];
      def cost: Rep[Long]
    };
    trait CostedBuilder extends Def[CostedBuilder];
    trait CostedCompanion;
    trait CostedBuilderCompanion
  }
}