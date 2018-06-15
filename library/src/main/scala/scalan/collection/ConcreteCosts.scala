package scalan.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    abstract class CostedPrim[Val](val value: Rep[Val], val cost: Rep[Long]) extends Costed[Val] {
      def builder: Rep[ConcreteCostedBuilder] = ConcreteCostedBuilder()
    };
    abstract class CostedPair[L, R](val l: Rep[L], val r: Rep[R], val cost: Rep[Long]) extends Costed[scala.Tuple2[L, R]] {
      def builder: Rep[ConcreteCostedBuilder] = ConcreteCostedBuilder();
      def value: Rep[scala.Tuple2[L, R]] = Pair(CostedPair.this.l, CostedPair.this.r)
    };
    abstract class CostedArray[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Long]]) extends Costed[WArray[Item]] {
      def builder: Rep[ConcreteCostedBuilder] = ConcreteCostedBuilder();
      def value: Rep[WArray[Item]] = CostedArray.this.values.arr;
      def cost: Rep[Long] = CostedArray.this.costs.fold[Long](toRep(0L.asInstanceOf[Long]))(fun(((p: Rep[scala.Tuple2[Long, Long]]) => p._1.+(p._2))))
    };
    abstract class ConcreteCostedBuilder extends CostedBuilder;
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait CostedArrayCompanion;
    trait ConcreteCostedBuilderCompanion
  }
}