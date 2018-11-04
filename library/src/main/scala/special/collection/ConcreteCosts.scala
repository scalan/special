package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import CCostedBuilder._;
    import Col._;
    import Costed._;
    import CostedBuilder._;
    import CostedCol._;
    import CostedFunc._;
    import CostedNestedCol._;
    import CostedPair._;
    import CostedPairCol._;
    import CostedPrim._;
    import CostedSum._;
    import MonoidBuilderInst._;
    abstract class CCostedPrim[Val](val value: Rep[Val], val cost: Rep[Int], val dataSize: Rep[Long]) extends CostedPrim[Val] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder()
    };
    abstract class CCostedPair[L, R](val l: Rep[Costed[L]], val r: Rep[Costed[R]]) extends CostedPair[L, R] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[scala.Tuple2[L, R]] = Pair(CCostedPair.this.l.value, CCostedPair.this.r.value);
      def cost: Rep[Int] = CCostedPair.this.l.cost.+(CCostedPair.this.r.cost).+(CCostedPair.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CCostedPair.this.l.dataSize.+(CCostedPair.this.r.dataSize)
    };
    abstract class CCostedSum[L, R](val value: Rep[WEither[L, R]], val left: Rep[Costed[Unit]], val right: Rep[Costed[Unit]]) extends CostedSum[L, R] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CCostedFunc[Env, Arg, Res](val envCosted: Rep[Costed[Env]], val func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], val cost: Rep[Int], val dataSize: Rep[Long]) extends CostedFunc[Env, Arg, Res] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def value: Rep[scala.Function1[Arg, Res]] = delayInvoke
    };
    abstract class CCostedCol[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Int]], val sizes: Rep[Col[Long]], val valuesCost: Rep[Int]) extends CostedCol[Item] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Col[Item]] = CCostedCol.this.values;
      def cost: Rep[Int] = CCostedCol.this.valuesCost.+(CCostedCol.this.costs.sum(CCostedCol.this.builder.monoidBuilder.intPlusMonoid));
      def dataSize: Rep[Long] = CCostedCol.this.sizes.sum(CCostedCol.this.builder.monoidBuilder.longPlusMonoid);
      @NeverInline def mapCosted[Res](f: Rep[scala.Function1[Costed[Item], Costed[Res]]]): Rep[CostedCol[Res]] = delayInvoke;
      @NeverInline def filterCosted(f: Rep[scala.Function1[Costed[Item], Costed[Boolean]]]): Rep[CostedCol[Item]] = delayInvoke;
      @NeverInline def foldCosted[B](zero: Rep[Costed[B]], op: Rep[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Rep[Nothing] = delayInvoke
    };
    abstract class CCostedPairCol[L, R](val ls: Rep[Costed[Col[L]]], val rs: Rep[Costed[Col[R]]]) extends CostedPairCol[L, R] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Col[scala.Tuple2[L, R]]] = CCostedPairCol.this.ls.value.zip[R](CCostedPairCol.this.rs.value);
      def cost: Rep[Int] = CCostedPairCol.this.ls.cost.+(CCostedPairCol.this.rs.cost).+(CCostedPairCol.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CCostedPairCol.this.ls.dataSize.+(CCostedPairCol.this.rs.dataSize)
    };
    abstract class CCostedNestedCol[Item](val rows: Rep[Col[Costed[Col[Item]]]]) extends CostedNestedCol[Item] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Col[Col[Item]]] = CCostedNestedCol.this.rows.map[Col[Item]](fun(((r: Rep[Costed[Col[Item]]]) => r.value)));
      def cost: Rep[Int] = CCostedNestedCol.this.rows.map[Int](fun(((r: Rep[Costed[Col[Item]]]) => r.cost)))(((ClassTag.Int): ClassTag[Int])).sum(CCostedNestedCol.this.builder.monoidBuilder.intPlusMonoid);
      def dataSize: Rep[Long] = CCostedNestedCol.this.rows.map[Long](fun(((r: Rep[Costed[Col[Item]]]) => r.dataSize)))(((ClassTag.Long): ClassTag[Long])).sum(CCostedNestedCol.this.builder.monoidBuilder.longPlusMonoid)
    };
    abstract class CCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = RMonoidBuilderInst();
      @NeverInline def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = delayInvoke
    };
    trait CCostedPrimCompanion;
    trait CCostedPairCompanion;
    trait CCostedSumCompanion;
    trait CCostedFuncCompanion;
    trait CCostedColCompanion;
    trait CCostedPairColCompanion;
    trait CCostedNestedColCompanion;
    trait CCostedBuilderCompanion
  }
}