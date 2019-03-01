package special.collection {
  import scalan._

  trait CostedOptions extends Base { self: Library =>
    import CCostedBuilder._;
    import CCostedOption._;
    import CCostedPrim._;
    import Costed._;
    import CostedBuilder._;
    import CostedNone._;
    import CostedOption._;
    import CostedPrim._;
    import CostedSome._;
    import WOption._;
    import WSpecialPredef._;
    abstract class CostedSome[T](val costedValue: Rep[Costed[T]]) extends CostedOption[T] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[WOption[T]] = RWSpecialPredef.some[T](CostedSome.this.costedValue.value);
      def dataSize: Rep[Long] = CostedSome.this.builder.SumTagSize.+(CostedSome.this.costedValue.dataSize);
      def cost: Rep[Int] = CostedSome.this.costedValue.cost.+(CostedSome.this.builder.ConstructSumCost);
      def get: Rep[Costed[T]] = CostedSome.this.costedValue;
      def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = CostedSome.this.costedValue;
      @NeverInline def fold[B](ifEmpty: Rep[Costed[B]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      def isEmpty: Rep[Costed[Boolean]] = RCCostedPrim(toRep(false.asInstanceOf[Boolean]), CostedSome.this.costedValue.cost.+(CostedSome.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      def isDefined: Rep[Costed[Boolean]] = RCCostedPrim(toRep(true.asInstanceOf[Boolean]), CostedSome.this.costedValue.cost.+(CostedSome.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    abstract class CostedNone[T](val cost: Rep[Int])(implicit val eT: Elem[T]) extends CostedOption[T] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      def value: Rep[WOption[T]] = RWSpecialPredef.none[T];
      def dataSize: Rep[Long] = CostedNone.this.builder.SumTagSize;
      def get: Rep[Costed[T]] = CostedNone.this.builder.costedValue[T](CostedNone.this.builder.defaultValue[T](CostedNone.this.eT), RWSpecialPredef.some[Int](CostedNone.this.cost));
      @NeverInline def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def fold[B](ifEmpty: Rep[Costed[B]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      def isEmpty: Rep[Costed[Boolean]] = RCCostedPrim(toRep(true.asInstanceOf[Boolean]), CostedNone.this.cost.+(CostedNone.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      def isDefined: Rep[Costed[Boolean]] = RCCostedPrim(toRep(false.asInstanceOf[Boolean]), CostedNone.this.cost.+(CostedNone.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    abstract class CCostedOption[T](val value: Rep[WOption[T]], val costOpt: Rep[WOption[Int]], val sizeOpt: Rep[WOption[Long]], val accumulatedCost: Rep[Int]) extends CostedOption[T] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      // manual fix
      def cost: Rep[Int] = CCostedOption.this.accumulatedCost.+(CCostedOption.this.costOpt.getOrElse[Int](Thunk(toRep(0.asInstanceOf[Int]))));
      // manual fix
      def dataSize: Rep[Long] = sizeData(value.elem, sizeOpt);
      def get: Rep[Costed[T]] = CCostedOption.this.builder.mkCostedPrim[T](CCostedOption.this.value.get, CCostedOption.this.cost, CCostedOption.this.dataSize);
      def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = {
        val v: Rep[T] = CCostedOption.this.value.getOrElse[T](default.value);
        val c: Rep[Int] = CCostedOption.this.accumulatedCost.+(CCostedOption.this.costOpt.getOrElse[Int](default.cost));
        val s: Rep[Long] = CCostedOption.this.sizeOpt.getOrElse[Long](default.dataSize);
        CCostedOption.this.builder.mkCostedPrim[T](v, c, s)
      };
      def isEmpty: Rep[Costed[Boolean]] = CCostedOption.this.builder.mkCostedPrim[Boolean](CCostedOption.this.value.isEmpty, CCostedOption.this.cost, toRep(1L.asInstanceOf[Long]));
      def isDefined: Rep[Costed[Boolean]] = CCostedOption.this.builder.mkCostedPrim[Boolean](CCostedOption.this.value.isDefined, CCostedOption.this.cost, toRep(1L.asInstanceOf[Long]));
      @NeverInline def fold[B](ifEmpty: Rep[Costed[B]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    trait CostedSomeCompanion;
    trait CostedNoneCompanion;
    trait CCostedOptionCompanion
  }
}