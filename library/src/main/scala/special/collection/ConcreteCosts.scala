package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import ConcreteCostedBuilder._;
    import Costed._;
    import ConcreteCosted._;
    import WEither._;
    import WArray._;
    import Col._;
    import CostedCol._;
    import MonoidBuilderInst._;
    import CostedBuilder._;
    trait ConcreteCosted[Val] extends Costed[Val] {
      implicit def eVal: Elem[Val];
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder()
    };
    abstract class CostedPrim[Val](val value: Rep[Val], val cost: Rep[Int], val dataSize: Rep[Long]) extends ConcreteCosted[Val];
    abstract class CostedPair[L, R](val l: Rep[Costed[L]], val r: Rep[Costed[R]]) extends ConcreteCosted[scala.Tuple2[L, R]] {
      def value: Rep[scala.Tuple2[L, R]] = Pair(CostedPair.this.l.value, CostedPair.this.r.value);
      def cost: Rep[Int] = CostedPair.this.l.cost.+(CostedPair.this.r.cost).+(CostedPair.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CostedPair.this.l.dataSize.+(CostedPair.this.r.dataSize)
    };
    abstract class CostedSum[L, R](val value: Rep[WEither[L, R]], val left: Rep[Costed[Unit]], val right: Rep[Costed[Unit]]) extends ConcreteCosted[WEither[L, R]] {
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CostedFunc[Env, Arg, Res](val envCosted: Rep[Costed[Env]], val func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], val cost: Rep[Int], val dataSize: Rep[Long]) extends ConcreteCosted[scala.Function1[Arg, Res]] {
      implicit def eArg: Elem[Arg]
      @NeverInline def value: Rep[scala.Function1[Arg, Res]] = delayInvoke
    };
    abstract class CostedArray[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Int]], val sizes: Rep[Col[Long]]) extends ConcreteCosted[WArray[Item]] {
      def value: Rep[WArray[Item]] = CostedArray.this.values.arr;
      def cost: Rep[Int] = CostedArray.this.costs.sum(CostedArray.this.builder.monoidBuilder.intPlusMonoid);
      def dataSize: Rep[Long] = CostedArray.this.sizes.sum(CostedArray.this.builder.monoidBuilder.longPlusMonoid)
    };
    abstract class CostedCol[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Int]], val sizes: Rep[Col[Long]], val valuesCost: Rep[Int]) extends ConcreteCosted[Col[Item]] {
      def value: Rep[Col[Item]] = CostedCol.this.values;
      def cost: Rep[Int] = CostedCol.this.valuesCost.+(CostedCol.this.costs.sum(CostedCol.this.builder.monoidBuilder.intPlusMonoid));
      def dataSize: Rep[Long] = CostedCol.this.sizes.sum(CostedCol.this.builder.monoidBuilder.longPlusMonoid);
      @NeverInline def mapCosted[Res](f: Rep[scala.Function1[Costed[Item], Costed[Res]]]): Rep[CostedCol[Res]] = delayInvoke;
      @NeverInline def filterCosted(f: Rep[scala.Function1[Costed[Item], Costed[Boolean]]]): Rep[CostedCol[Item]] = delayInvoke
    };
    abstract class CostedPairArray[L, R](val ls: Rep[Costed[WArray[L]]], val rs: Rep[Costed[WArray[R]]]) extends ConcreteCosted[WArray[scala.Tuple2[L, R]]] {
      def value: Rep[WArray[scala.Tuple2[L, R]]] = CostedPairArray.this.ls.value.zip(CostedPairArray.this.rs.value);
      def cost: Rep[Int] = CostedPairArray.this.ls.cost.+(CostedPairArray.this.rs.cost).+(CostedPairArray.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CostedPairArray.this.ls.dataSize.+(CostedPairArray.this.rs.dataSize)
    };
    abstract class CostedPairCol[L, R](val ls: Rep[Costed[Col[L]]], val rs: Rep[Costed[Col[R]]]) extends ConcreteCosted[Col[scala.Tuple2[L, R]]] {
      def value: Rep[Col[scala.Tuple2[L, R]]] = CostedPairCol.this.ls.value.zip[R](CostedPairCol.this.rs.value);
      def cost: Rep[Int] = CostedPairCol.this.ls.cost.+(CostedPairCol.this.rs.cost).+(CostedPairCol.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CostedPairCol.this.ls.dataSize.+(CostedPairCol.this.rs.dataSize)
    };
    abstract class CostedNestedArray[Item](val rows: Rep[Col[Costed[WArray[Item]]]]) extends ConcreteCosted[WArray[WArray[Item]]] {
      implicit def eItem: Elem[Item]
      def value: Rep[WArray[WArray[Item]]] = CostedNestedArray.this.rows.map[WArray[Item]](fun(((r: Rep[Costed[WArray[Item]]]) => r.value))).arr;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CostedNestedCol[Item](val rows: Rep[Col[Costed[Col[Item]]]]) extends ConcreteCosted[Col[Col[Item]]] {
      implicit def eItem: Elem[Item]
      def value: Rep[Col[Col[Item]]] = CostedNestedCol.this.rows.map[Col[Item]](fun(((r: Rep[Costed[Col[Item]]]) => r.value)));
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class ConcreteCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = RMonoidBuilderInst();
      @NeverInline def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = delayInvoke
    };
    trait ConcreteCostedCompanion;
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait CostedSumCompanion;
    trait CostedFuncCompanion;
    trait CostedArrayCompanion;
    trait CostedColCompanion;
    trait CostedPairArrayCompanion;
    trait CostedPairColCompanion;
    trait CostedNestedArrayCompanion;
    trait CostedNestedColCompanion;
    trait ConcreteCostedBuilderCompanion
  }
}