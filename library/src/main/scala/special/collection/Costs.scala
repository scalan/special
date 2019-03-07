package special.collection {
  import scalan._

  trait Costs extends Base { self: Library =>
    import Coll._;
    import Costed._;
    import CostedBuilder._;
    import CostedColl._;
    import CostedFunc._;
    import CostedOption._;
    import CostedPair._;
    import CostedPrim._;
    import MonoidBuilder._;
    import Size._;
    import SizeColl._;
    import SizeFunc._;
    import SizeOption._;
    import SizePair._;
    import SizePrim._;
    import WOption._;
    import WRType._;
    trait Costed[Val] extends Def[Costed[Val]] {
      implicit def eVal: Elem[Val];
      def builder: Rep[CostedBuilder];
      def value: Rep[Val];
      def cost: Rep[Int];
      def size: Rep[Size[Val]]
    };
    trait CostedPrim[Val] extends Costed[Val] {
      implicit def eVal: Elem[Val];
      def value: Rep[Val];
      def cost: Rep[Int];
      def size: Rep[Size[Val]]
    };
    trait CostedPair[L, R] extends Costed[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def l: Rep[Costed[L]];
      def r: Rep[Costed[R]]
    };
    trait CostedFunc[Env, Arg, Res] extends Costed[scala.Function1[Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def envCosted: Rep[Costed[Env]];
      def func: Rep[scala.Function1[Costed[Arg], Costed[Res]]];
      def cost: Rep[Int]
    };
    trait CostedColl[Item] extends Costed[Coll[Item]] {
      implicit def eItem: Elem[Item];
      def values: Rep[Coll[Item]];
      def costs: Rep[Coll[Int]];
      def sizes: Rep[Coll[Size[Item]]];
      def valuesCost: Rep[Int];
      def mapCosted[Res](f: Rep[scala.Function1[Costed[Item], Costed[Res]]]): Rep[CostedColl[Res]];
      def filterCosted(f: Rep[scala.Function1[Costed[Item], Costed[Boolean]]]): Rep[CostedColl[Item]];
      def foldCosted[B](zero: Rep[Costed[B]], op: Rep[scala.Function1[Costed[scala.Tuple2[B, Item]], Costed[B]]]): Rep[Costed[B]]
    };
    trait CostedOption[T] extends Costed[WOption[T]] {
      implicit def eT: Elem[T];
      def costOpt: Rep[WOption[Int]];
      def sizeOpt: Rep[WOption[Size[T]]];
      def accumulatedCost: Rep[Int]
    };
    trait CostedBuilder extends Def[CostedBuilder] {
      def ConstructTupleCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def ConstructSumCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def SelectFieldCost: Rep[Int] = toRep(1.asInstanceOf[Int]);
      def SumTagSize: Rep[Long] = toRep(1L.asInstanceOf[Long]);
      def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]];
      def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T];
      def monoidBuilder: Rep[MonoidBuilder];
      def mkSizePrim[T](dataSize: Rep[Long], tT: Rep[WRType[T]]): Rep[SizePrim[T]];
      def mkSizePair[L, R](l: Rep[Size[L]], r: Rep[Size[R]]): Rep[SizePair[L, R]];
      def mkSizeColl[T](sizes: Rep[Coll[Size[T]]]): Rep[SizeColl[T]];
      def mkSizeFunc[E, A, R](sizeEnv: Rep[Size[E]], sizeFunc: Rep[Long], tA: Rep[WRType[A]], tR: Rep[WRType[R]]): Rep[SizeFunc[E, A, R]];
      def mkSizeOption[T](sizeOpt: Rep[WOption[Size[T]]]): Rep[SizeOption[T]];
      def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Size[T]]): Rep[CostedPrim[T]];
      def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]]): Rep[CostedPair[L, R]];
      def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[scala.Function1[Costed[Arg], Costed[Res]]], cost: Rep[Int], size: Rep[Size[scala.Function1[Arg, Res]]]): Rep[CostedFunc[Env, Arg, Res]];
      def mkCostedColl[T](values: Rep[Coll[T]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[T]]], valuesCost: Rep[Int]): Rep[CostedColl[T]];
      def mkCostedOption[T](value: Rep[WOption[T]], costOpt: Rep[WOption[Int]], sizeOpt: Rep[WOption[Size[T]]], accumulatedCost: Rep[Int]): Rep[CostedOption[T]]
    };
    trait CostedCompanion;
    trait CostedPrimCompanion;
    trait CostedPairCompanion;
    trait CostedFuncCompanion;
    trait CostedCollCompanion;
    trait CostedOptionCompanion;
    trait CostedBuilderCompanion
  }
}