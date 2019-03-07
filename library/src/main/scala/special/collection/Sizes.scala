package special.collection {
  import scalan._

  trait Sizes extends Base { self: Library =>
    import Coll._;
    import Size._;
    import WOption._;
    @Liftable trait Size[Val] extends Def[Size[Val]] {
      implicit def eVal: Elem[Val];
      def dataSize: Rep[Long]
    };
    @Liftable trait SizePrim[Val] extends Size[Val] {
      implicit def eVal: Elem[Val];
      def dataSize: Rep[Long]
    };
    @Liftable trait SizePair[L, R] extends Size[scala.Tuple2[L, R]] {
      implicit def eL: Elem[L];
      implicit def eR: Elem[R];
      def l: Rep[Size[L]];
      def r: Rep[Size[R]]
    };
    @Liftable trait SizeColl[Item] extends Size[Coll[Item]] {
      implicit def eItem: Elem[Item];
      def sizes: Rep[Coll[Size[Item]]]
    };
    @Liftable trait SizeFunc[Env, Arg, Res] extends Size[scala.Function1[Arg, Res]] {
      implicit def eEnv: Elem[Env];
      implicit def eArg: Elem[Arg];
      implicit def eRes: Elem[Res];
      def sizeEnv: Rep[Size[Env]]
    };
    @Liftable trait SizeOption[T] extends Size[WOption[T]] {
      implicit def eT: Elem[T];
      def sizeOpt: Rep[WOption[Size[T]]]
    };
    trait SizeCompanion;
    trait SizePrimCompanion;
    trait SizePairCompanion;
    trait SizeCollCompanion;
    trait SizeFuncCompanion;
    trait SizeOptionCompanion
  }
}