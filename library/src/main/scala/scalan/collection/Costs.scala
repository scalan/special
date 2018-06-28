package scalan.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    trait Costed[Val] extends Def[Costed[Val]] {
      implicit def eVal: Elem[Val];
      def builder: Rep[CostedBuilder];
      def value: Rep[Val];
      def cost: Rep[Int]
    };
    trait CostedBuilder extends Def[CostedBuilder];
    trait CostedCompanion;
    trait CostedBuilderCompanion
  }
}