package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import Closure._;
    import ConcreteCostedBuilder._;
    import Costed._;
    import CostedOption._;
    import WOption._;
    import WEither._;
    import WArray._;
    import Col._;
    import MonoidBuilderInst._;
    import CostedBuilder._;
    import WSpecialPredef._;
    trait Closure[Env, Arg, Res] extends Def[Closure[Env, Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def env: Rep[Env];
      @OverloadId(value = "apply_with_env") def apply(e: Rep[Env], a: Rep[Arg]): Rep[Res];
      @OverloadId(value = "apply") def apply(a: Rep[Arg]): Rep[Res] = this.apply(Closure.this.env, a)
    };
    abstract class CostedPrim[Val](val value: Rep[Val], val cost: Rep[Int]) extends Costed[Val] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder()
    };
    abstract class CostedPair[L, R](val l: Rep[Costed[L]], val r: Rep[Costed[R]]) extends Costed[scala.Tuple2[L, R]] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[scala.Tuple2[L, R]] = Pair(CostedPair.this.l.value, CostedPair.this.r.value);
      def cost: Rep[Int] = CostedPair.this.l.cost.+(CostedPair.this.r.cost).+(toRep(1.asInstanceOf[Int]))
    };
    abstract class ClosureBase[Env, Arg, Res](val env: Rep[Env], val func: Rep[scala.Function1[scala.Tuple2[Env, Arg], Res]]) extends Closure[Env, Arg, Res] {
      def apply(e: Rep[Env], a: Rep[Arg]): Rep[Res] = ClosureBase.this.func.apply(Pair(e, a))
    };
    abstract class CostedFunc[Env, Arg, Res](val envCost: Rep[Costed[Env]], val func: Rep[Closure[Env, Arg, Res]], val costFunc: Rep[Closure[Env, Arg, Long]]) extends Costed[scala.Function1[Arg, Res]] {
      implicit def eArg: Elem[Arg]
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[scala.Function1[Arg, Res]] = fun(((a: Rep[Arg]) => CostedFunc.this.func.apply(a)));
      def cost: Rep[Int] = CostedFunc.this.envCost.cost.+(toRep(1.asInstanceOf[Int]))
    };
    abstract class CostedOption[T](val either: Rep[WEither[Int, Costed[T]]]) extends Costed[WOption[T]] {
      implicit def eT: Elem[T]
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[WOption[T]] = CostedOption.this.either.fold[WOption[T]](fun(((l: Rep[Int]) => RWSpecialPredef.none[T])), fun(((r: Rep[Costed[T]]) => RWSpecialPredef.some[T](r.value))));
      def cost: Rep[Int] = CostedOption.this.either.fold[Int](fun(((l: Rep[Int]) => l)), fun(((r: Rep[Costed[T]]) => r.cost)))
    };
    abstract class CostedArray[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Int]]) extends Costed[WArray[Item]] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[WArray[Item]] = CostedArray.this.values.arr;
      def cost: Rep[Int] = CostedArray.this.costs.sum(CostedArray.this.builder.monoidBuilder.intPlusMonoid)
    };
    abstract class CostedCol[Item](val values: Rep[Col[Item]], val costs: Rep[Col[Int]]) extends Costed[Col[Item]] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[Col[Item]] = CostedCol.this.values;
      def cost: Rep[Int] = CostedCol.this.costs.sum(CostedCol.this.builder.monoidBuilder.intPlusMonoid)
    };
    abstract class CostedPairArray[L, R](val ls: Rep[Costed[WArray[L]]], val rs: Rep[Costed[WArray[R]]]) extends Costed[WArray[scala.Tuple2[L, R]]] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[WArray[scala.Tuple2[L, R]]] = CostedPairArray.this.ls.value.zip(CostedPairArray.this.rs.value);
      def cost: Rep[Int] = CostedPairArray.this.ls.cost.+(CostedPairArray.this.rs.cost).+(CostedPairArray.this.ls.value.length)
    };
    abstract class CostedPairCol[L, R](val ls: Rep[Costed[Col[L]]], val rs: Rep[Costed[Col[R]]]) extends Costed[Col[scala.Tuple2[L, R]]] {
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[Col[scala.Tuple2[L, R]]] = CostedPairCol.this.ls.value.zip[R](CostedPairCol.this.rs.value);
      def cost: Rep[Int] = CostedPairCol.this.ls.cost.+(CostedPairCol.this.rs.cost).+(CostedPairCol.this.ls.value.length)
    };
    abstract class CostedNestedArray[Item](val rows: Rep[Col[Costed[WArray[Item]]]]) extends Costed[WArray[WArray[Item]]] {
      implicit def eItem: Elem[Item]
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[WArray[WArray[Item]]] = CostedNestedArray.this.rows.map[WArray[Item]](fun(((r: Rep[Costed[WArray[Item]]]) => r.value))).arr;
      def cost: Rep[Int] = CostedNestedArray.this.rows.map[Int](fun(((r: Rep[Costed[WArray[Item]]]) => r.cost))).sum(CostedNestedArray.this.builder.monoidBuilder.intPlusMonoid)
    };
    abstract class CostedNestedCol[Item](val rows: Rep[Col[Costed[Col[Item]]]]) extends Costed[Col[Col[Item]]] {
      implicit def eItem: Elem[Item]
      def builder: Rep[ConcreteCostedBuilder] = RConcreteCostedBuilder();
      def value: Rep[Col[Col[Item]]] = CostedNestedCol.this.rows.map[Col[Item]](fun(((r: Rep[Costed[Col[Item]]]) => r.value)));
      def cost: Rep[Int] = CostedNestedCol.this.rows.map[Int](fun(((r: Rep[Costed[Col[Item]]]) => r.cost))).sum(CostedNestedCol.this.builder.monoidBuilder.intPlusMonoid)
    };
    abstract class ConcreteCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = RMonoidBuilderInst()
    };
    trait ClosureCompanion;
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait ClosureBaseCompanion;
    trait CostedFuncCompanion;
    trait CostedOptionCompanion;
    trait CostedArrayCompanion;
    trait CostedColCompanion;
    trait CostedPairArrayCompanion;
    trait CostedPairColCompanion;
    trait CostedNestedArrayCompanion;
    trait CostedNestedColCompanion;
    trait ConcreteCostedBuilderCompanion
  }
}