package special.collection {
  import scalan._

  trait CostedOptions extends Base { self: Library =>
    import Costed._;
    import WOption._;
    import ConcreteCosted._;
    import CostedPrim._;
    import CostedOption._;
    import WSpecialPredef._  // manual fix
    import ColBuilder._  // manual fix
    import ConcreteCostedBuilder._  // manual fix
    import Liftables._  // manual fix

    trait CostedOption[T] extends ConcreteCosted[WOption[T]] {
      implicit def eT: Elem[T];
      def get: Rep[Costed[T]];
      def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]];
      def fold[B](ifEmpty: Rep[Costed[Function0[B]]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]];
      def isEmpty: Rep[Costed[Boolean]];
      def isDefined: Rep[Costed[Boolean]];
      def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]];
      def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]];
      def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]]
    };
    abstract class CostedSome[T](val costedValue: Rep[Costed[T]]) extends CostedOption[T] {
      def value: Rep[WOption[T]] = RWSpecialPredef.some[T](CostedSome.this.costedValue.value);
      def dataSize: Rep[Long] = CostedSome.this.builder.SumTagSize.+(CostedSome.this.costedValue.dataSize);
      def cost: Rep[Int] = CostedSome.this.costedValue.cost.+(CostedSome.this.builder.ConstructSumCost);
      def get: Rep[Costed[T]] = CostedSome.this.costedValue;
      def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = CostedSome.this.costedValue;
      @NeverInline def fold[B](ifEmpty: Rep[Costed[Function0[B]]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      def isEmpty: Rep[Costed[Boolean]] = RCostedPrim(toRep(false.asInstanceOf[Boolean]), CostedSome.this.costedValue.cost.+(CostedSome.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      def isDefined: Rep[Costed[Boolean]] = RCostedPrim(toRep(true.asInstanceOf[Boolean]), CostedSome.this.costedValue.cost.+(CostedSome.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    abstract class CostedNone[T](val cost: Rep[Int])(implicit val eT: Elem[T]) extends CostedOption[T] {
      def value: Rep[WOption[T]] = RWSpecialPredef.none[T];
      def dataSize: Rep[Long] = CostedNone.this.builder.SumTagSize;
      // manual fix (liftElem)
      def get: Rep[Costed[T]] = CostedNone.this.builder.costedValue[T](CostedNone.this.builder.defaultValue[T](liftElem(CostedNone.this.eT)), RWSpecialPredef.some[Int](CostedNone.this.cost));
      @NeverInline def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = delayInvoke;
      @NeverInline def fold[B](ifEmpty: Rep[Costed[Function0[B]]], f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[B]] = delayInvoke;
      def isEmpty: Rep[Costed[Boolean]] = RCostedPrim(toRep(true.asInstanceOf[Boolean]), CostedNone.this.cost.+(CostedNone.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      def isDefined: Rep[Costed[Boolean]] = RCostedPrim(toRep(false.asInstanceOf[Boolean]), CostedNone.this.cost.+(CostedNone.this.builder.SelectFieldCost), toRep(1L.asInstanceOf[Long]));
      @NeverInline def filter(p: Rep[Costed[scala.Function1[T, Boolean]]]): Rep[Costed[WOption[T]]] = delayInvoke;
      @NeverInline def flatMap[B](f: Rep[Costed[scala.Function1[T, WOption[B]]]]): Rep[Costed[WOption[B]]] = delayInvoke;
      @NeverInline def map[B](f: Rep[Costed[scala.Function1[T, B]]]): Rep[Costed[WOption[B]]] = delayInvoke
    };
    trait CostedOptionCompanion;
    trait CostedSomeCompanion;
    trait CostedNoneCompanion
  }
}