package special.collection {
  import scalan._

  trait ConcreteCosts extends Base { self: Library =>
    import CCostedBuilder._;
    import CCostedColl._;
    import CCostedFunc._;
    import CCostedOption._;
    import CCostedPair._;
    import CCostedPrim._;
    import CSizeColl._;
    import CSizeFunc._;
    import CSizeOption._;
    import CSizePair._;
    import CSizePrim._;
    import Coll._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedFunc._;
    import CostedOption._;
    import CostedPair._;
    import CostedPrim._;
    import MonoidBuilderInst._;
    import Size._;
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    import WOption._;
    import WRType._;
    abstract class CCostedPrim[Val](val value: Rep[Val], val cost: Rep[Int], val size: Rep[Size[Val]]) extends CostedPrim[Val] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder()
    };
    abstract class CCostedPair[L, R](val l: Rep[Costed[L]], val r: Rep[Costed[R]], val accCost: Rep[Int]) extends CostedPair[L, R] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[scala.Tuple2[L, R]] = Pair(CCostedPair.this.l.value, CCostedPair.this.r.value);
      @NeverInline def cost: Rep[Int] = delayInvoke;
      def size: Rep[Size[scala.Tuple2[L, R]]] = CCostedPair.this.builder.mkSizePair[L, R](CCostedPair.this.l.size, CCostedPair.this.r.size)
    };
    abstract class CCostedFunc[Env, Arg, Res](val envCosted: Rep[Costed[Env]], val func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], val cost: Rep[Int], val size: Rep[Size[scala.Function1[Arg, Res]]]) extends CostedFunc[Env, Arg, Res] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def value: Rep[scala.Function1[Arg, Res]] = delayInvoke

      // manual fix
      lazy val sliceCalc: Rep[Arg => Res] = fun { x: Rep[Arg] => func(RCCostedPrim(x, 0, zeroSize(x.elem))).value }

      // manual fix
      lazy val sliceCost: Rep[((Int,Size[Arg])) => Int] = fun { in: Rep[(Int, Size[Arg])] =>
        val Pair(c, s) = in
        func(RCCostedPrim(placeholder[Arg], c, s)).cost
      }

      // manual fix
      lazy val sliceCostEx: Rep[((Arg, (Int,Size[Arg]))) => Int] = fun { in: Rep[(Arg, (Int, Size[Arg]))] =>
        val Pair(ctx, Pair(c, s)) = in
        func(RCCostedPrim(ctx, c, s)).cost
      }

      // manual fix
      lazy val sliceSize: Rep[Size[Arg] => Size[Res]] = fun { in: Rep[Size[Arg]] =>
        val s = in
        val arg = RCCostedPrim(placeholder[Arg], 0, s)
        func(arg).size
      }
    };
    abstract class CCostedColl[Item](val values: Rep[Coll[Item]], val costs: Rep[Coll[Int]], val sizes: Rep[Coll[Size[Item]]], val valuesCost: Rep[Int]) extends CostedColl[Item] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[Coll[Item]] = CCostedColl.this.values;
      @NeverInline def cost: Rep[Int] = delayInvoke;
      def size: Rep[Size[Coll[Item]]] = CCostedColl.this.builder.mkSizeColl[Item](CCostedColl.this.sizes);
      @NeverInline def mapCosted[Res](f: Rep[scala.Function1[Costed[Item], Costed[Res]]]): Rep[CostedColl[Res]] = delayInvoke;
      @NeverInline def filterCosted(f: Rep[scala.Function1[Costed[Item], Costed[Boolean]]]): Rep[CostedColl[Item]] = delayInvoke;
      @NeverInline def foldCosted[B](zero: Rep[Costed[B]], op: Rep[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Rep[Costed[B]] = delayInvoke
    };
    abstract class CCostedBuilder extends CostedBuilder {
      def monoidBuilder: Rep[MonoidBuilderInst] = RMonoidBuilderInst();
      @NeverInline def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = delayInvoke;
      def mkSizePrim[T](dataSize: Rep[Long], tT: Rep[WRType[T]]): Rep[SizePrim[T]] = RCSizePrim(dataSize, tT);
      def mkSizePair[L, R](l: Rep[Size[L]], r: Rep[Size[R]]): Rep[SizePair[L, R]] = RCSizePair(l, r);
      def mkSizeColl[T](sizes: Rep[Coll[Size[T]]]): Rep[SizeColl[T]] = RCSizeColl(sizes);
      def mkSizeFunc[E, A, R](sizeEnv: Rep[Size[E]], sizeFunc: Rep[Long], tA: Rep[WRType[A]], tR: Rep[WRType[R]]): Rep[SizeFunc[E, A, R]] = RCSizeFunc(sizeEnv, sizeFunc, tA, tR);
      def mkSizeOption[T](sizeOpt: Rep[WOption[Size[T]]]): Rep[SizeOption[T]] = RCSizeOption(sizeOpt);
      def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Size[T]]): Rep[CostedPrim[T]] = RCCostedPrim(value, cost, size);
      def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]], accCost: Rep[Int]): Rep[CostedPair[L, R]] = RCCostedPair(first, second, accCost);
      def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], cost: Rep[Int], size: Rep[Size[scala.Function1[Arg, Res]]]): Rep[CostedFunc[Env, Arg, Res]] = RCCostedFunc(envCosted, func, cost, size);
      def mkCostedColl[T](values: Rep[Coll[T]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[T]]], valuesCost: Rep[Int]): Rep[CostedColl[T]] = RCCostedColl(values, costs, sizes, valuesCost);
      def mkCostedOption[T](value: Rep[WOption[T]], costOpt: Rep[WOption[Int]], sizeOpt: Rep[WOption[Size[T]]], accumulatedCost: Rep[Int]): Rep[CostedOption[T]] = RCCostedOption(value, costOpt, sizeOpt, accumulatedCost)
    };
    trait CCostedPrimCompanion;
    trait CCostedPairCompanion;
    trait CCostedFuncCompanion;
    trait CCostedCollCompanion;
    trait CCostedBuilderCompanion
  }
}