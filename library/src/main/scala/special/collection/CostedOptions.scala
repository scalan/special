package special.collection {
  import scalan._

  trait CostedOptions extends Base { self: Library =>
    import CCostedBuilder._;
    import CCostedPrim._;
    import Costed._;
    import CostedBuilder._;
    import CostedNone._;
    import CostedOption._;
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
    abstract class CCostedOption[T](val value: Rep[WOption[T]], val none: Rep[Costed[Unit]], val some: Rep[Costed[Unit]]) extends CostedOption[T] {
      def builder: Rep[CostedBuilder] = RCCostedBuilder();
      @NeverInline def cost: Rep[Int] = delayInvoke;
      @NeverInline def dataSize: Rep[Long] = delayInvoke;
      @NeverInline def get: Rep[Costed[T]] = delayInvoke;
      @NeverInline def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def fold[B](ifEmpty: Rep[Costed[B]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      @NeverInline def isEmpty: Rep[Costed[Boolean]] = delayInvoke;
      @NeverInline def isDefined: Rep[Costed[Boolean]] = delayInvoke;
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    trait CostedSomeCompanion;
    trait CostedNoneCompanion;
    trait CCostedOptionCompanion
  }
}