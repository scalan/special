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
      def cost: Rep[Long] = CostedArray.this.costs.sum(CostedArray.this.builder.monoidBuilder.longPlusMonoid)
    };
    abstract class CostedPairArray[L, R](val ls: Rep[Costed[WArray[L]]], val rs: Rep[Costed[WArray[R]]]) extends Costed[WArray[scala.Tuple2[L, R]]] {
      def builder: Rep[ConcreteCostedBuilder] = ConcreteCostedBuilder();
      def value: Rep[WArray[scala.Tuple2[L, R]]] = CostedPairArray.this.ls.value.zip(CostedPairArray.this.rs.value);
      def cost: Rep[Long] = CostedPairArray.this.ls.cost.+(CostedPairArray.this.rs.cost).+(CostedPairArray.this.ls.value.length.toLong)
    };
    abstract class CostedNestedArray[Item](val rows: Rep[Col[Costed[WArray[Item]]]]) extends Costed[WArray[WArray[Item]]] {
      implicit def eItem: Elem[Item]
      def builder: Rep[ConcreteCostedBuilder] = ConcreteCostedBuilder();
      def value: Rep[WArray[WArray[Item]]] = CostedNestedArray.this.rows.map[WArray[Item]](fun(((r: Rep[Costed[WArray[Item]]]) => r.value))).arr;
      def cost: Rep[Long] = CostedNestedArray.this.rows.map[Long](fun(((r: Rep[Costed[WArray[Item]]]) => r.cost))).sum(CostedNestedArray.this.builder.monoidBuilder.longPlusMonoid)
    };
    abstract class ConcreteCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = MonoidBuilderInst()
    };
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait CostedArrayCompanion;
    trait CostedPairArrayCompanion;
    trait CostedNestedArrayCompanion;
    trait ConcreteCostedBuilderCompanion
  }
}