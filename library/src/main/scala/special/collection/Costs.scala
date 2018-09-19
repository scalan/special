package special.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    import CostedBuilder._;
    import Costed._;
    trait Costed[Val] extends Def[Costed[Val]] {
      implicit def eVal: Elem[Val];
      def builder: Rep[CostedBuilder];
      def value: Rep[Val];
      def cost: Rep[Int];
      def dataSize: Rep[Long]
    };
    trait CostedBuilder extends Def[CostedBuilder] {
      def ConstructTupleCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def ConstructSumCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def SelectFieldCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def SumTagSize: Rep[Long] = toRep(1L.asInstanceOf[Long]);
      def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]];
      def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T]
    };
    trait CostedCompanion;
    trait CostedBuilderCompanion
  }
}