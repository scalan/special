package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import CCostedBuilder._;
    import CCostedColl._;
    import CCostedFunc._;
    import CCostedNestedColl._;
    import CCostedOption._;
    import CCostedPair._;
    import CCostedPairColl._;
    import CCostedPrim._;
    import CCostedSum._;
    import Coll._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedFunc._;
    import CostedNestedColl._;
    import CostedNone._;
    import CostedOption._;
    import CostedPair._;
    import CostedPairColl._;
    import CostedPrim._;
    import CostedSome._;
    import CostedSum._;
    import Monoid._;
    import MonoidBuilder._;
    import MonoidBuilderInst._;
    import WEither._;
    import WOption._;
    import WRType._;
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
    abstract class CCostedColl[Item](val values: Rep[Coll[Item]], val costs: Rep[Coll[Int]], val sizes: Rep[Coll[Long]], val valuesCost: Rep[Int]) extends CostedColl[Item] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Coll[Item]] = CCostedColl.this.values;
      def cost: Rep[Int] = CCostedColl.this.valuesCost.+(CCostedColl.this.costs.sum(CCostedColl.this.builder.monoidBuilder.intPlusMonoid));
      def dataSize: Rep[Long] = CCostedColl.this.sizes.sum(CCostedColl.this.builder.monoidBuilder.longPlusMonoid);
      @NeverInline def mapCosted[Res](f: Rep[scala.Function1[Costed[Item], Costed[Res]]]): Rep[CostedColl[Res]] = delayInvoke;
      @NeverInline def filterCosted(f: Rep[scala.Function1[Costed[Item], Costed[Boolean]]]): Rep[CostedColl[Item]] = delayInvoke;
      @NeverInline def foldCosted[B](zero: Rep[Costed[B]], op: Rep[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Rep[Costed[B]] = delayInvoke
    };
    abstract class CCostedPairColl[L, R](val ls: Rep[Costed[Coll[L]]], val rs: Rep[Costed[Coll[R]]]) extends CostedPairColl[L, R] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Coll[scala.Tuple2[L, R]]] = CCostedPairColl.this.ls.value.zip[R](CCostedPairColl.this.rs.value);
      def cost: Rep[Int] = CCostedPairColl.this.ls.cost.+(CCostedPairColl.this.rs.cost).+(CCostedPairColl.this.builder.ConstructTupleCost);
      def dataSize: Rep[Long] = CCostedPairColl.this.ls.dataSize.+(CCostedPairColl.this.rs.dataSize)
    };
    abstract class CCostedNestedColl[Item](val rows: Rep[Coll[Costed[Coll[Item]]]]) extends CostedNestedColl[Item] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def value: Rep[Coll[Coll[Item]]] = delayInvoke;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke
    };
    abstract class CCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = RMonoidBuilderInst();
      @NeverInline def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = delayInvoke;
      def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Long]): Rep[CostedPrim[T]] = RCCostedPrim(value, cost, size);
      def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]]): Rep[CostedPair[L, R]] = RCCostedPair(first, second);
      def mkCostedSum[L, R](value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CostedSum[L, R]] = RCCostedSum(value, left, right);
      def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedFunc[Env, Arg, Res]] = RCCostedFunc(envCosted, func, cost, dataSize);
      def mkCostedColl[T](values: Rep[Coll[T]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Long]], valuesCost: Rep[Int]): Rep[CostedColl[T]] = RCCostedColl(values, costs, sizes, valuesCost);
      def mkCostedPairColl[L, R](ls: Rep[Costed[Coll[L]]], rs: Rep[Costed[Coll[R]]]): Rep[CostedPairColl[L, R]] = RCCostedPairColl(ls, rs);
      def mkCostedNestedColl[Item](rows: Rep[Coll[Costed[Coll[Item]]]]): Rep[CostedNestedColl[Item]] = RCCostedNestedColl(rows);
      def mkCostedSome[T](costedValue: Rep[Costed[T]]): Rep[CostedOption[T]] = RCostedSome(costedValue);
      def mkCostedNone[T](cost: Rep[Int])(implicit eT: Elem[T]): Rep[CostedOption[T]] = RCostedNone(cost);
      def mkCostedOption[T](value: Rep[WOption[T]], costOpt: Rep[WOption[Int]], sizeOpt: Rep[WOption[Long]], accumulatedCost: Rep[Int]): Rep[CostedOption[T]] = RCCostedOption(value, costOpt, sizeOpt, accumulatedCost)
    };
    trait CCostedPrimCompanion;
    trait CCostedPairCompanion;
    trait CCostedSumCompanion;
    trait CCostedFuncCompanion;
    trait CCostedCollCompanion;
    trait CCostedPairCollCompanion;
    trait CCostedNestedCollCompanion;
    trait CCostedBuilderCompanion
  }
}