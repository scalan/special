package special.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    import CostedBuilder._;
    import Costed._;
    trait Costed[Val] extends Def[Costed[Val]] with Sized[Val] {
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