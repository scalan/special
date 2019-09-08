package special.collection {
  import scalan._

  trait MonoidInstances extends Base { self: Library =>
    import Monoid._;
    import MonoidBuilder._;
    abstract class MonoidBuilderInst extends MonoidBuilder {
      override def pairMonoid[A, B](m1: Ref[Monoid[A]], m2: Ref[Monoid[B]]): Ref[Monoid[scala.Tuple2[A, B]]] = ???
      val `intPlusMonoid ` : Ref[IntPlusMonoid] = ???
      def intPlusMonoid: Ref[IntPlusMonoid] = MonoidBuilderInst.this.`intPlusMonoid `;
      val `intMaxMonoid ` : Ref[IntMaxMonoid] = ???
      def intMaxMonoid: Ref[IntMaxMonoid] = MonoidBuilderInst.this.`intMaxMonoid `;
      val `intMinMonoid ` : Ref[IntMinMonoid] = ???
      def intMinMonoid: Ref[IntMinMonoid] = MonoidBuilderInst.this.`intMinMonoid `;
      val `longPlusMonoid ` : Ref[LongPlusMonoid] = ???
      def longPlusMonoid: Ref[LongPlusMonoid] = MonoidBuilderInst.this.`longPlusMonoid `;
      val `longMaxMonoid ` : Ref[LongMaxMonoid] = ???
      def longMaxMonoid: Ref[LongMaxMonoid] = MonoidBuilderInst.this.`longMaxMonoid `;
      val `longMinMonoid ` : Ref[LongMinMonoid] = ???
      def longMinMonoid: Ref[LongMinMonoid] = MonoidBuilderInst.this.`longMinMonoid `
    };
    abstract class IntPlusMonoid(val zero: Ref[Int]) extends Monoid[Int] {
      def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = x.+(y);
      def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = x.*(n)
    };
    abstract class IntMaxMonoid(val zero: Ref[Int]) extends Monoid[Int] {
      @NeverInline def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = delayInvoke;
      @NeverInline def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = delayInvoke
    };
    abstract class IntMinMonoid(val zero: Ref[Int]) extends Monoid[Int] {
      @NeverInline def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = delayInvoke;
      @NeverInline def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = delayInvoke
    };
    abstract class LongPlusMonoid(val zero: Ref[Long]) extends Monoid[Long] {
      def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = x.+(y);
      def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = x.*(n.toLong)
    };
    abstract class LongMaxMonoid(val zero: Ref[Long]) extends Monoid[Long] {
      @NeverInline def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = delayInvoke;
      @NeverInline def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = delayInvoke
    };
    abstract class LongMinMonoid(val zero: Ref[Long]) extends Monoid[Long] {
      @NeverInline def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = delayInvoke;
      @NeverInline def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = delayInvoke
    };
    abstract class PairMonoid[A, B](val m1: Ref[Monoid[A]], val m2: Ref[Monoid[B]]) extends Monoid[scala.Tuple2[A, B]] {
      override def zero: Ref[scala.Tuple2[A, B]] = Pair(PairMonoid.this.m1.zero, PairMonoid.this.m2.zero);
      override def plus(x: Ref[scala.Tuple2[A, B]], y: Ref[scala.Tuple2[A, B]]): Ref[scala.Tuple2[A, B]] = Pair(PairMonoid.this.m1.plus(x._1, y._1), PairMonoid.this.m2.plus(x._2, y._2));
      override def power(x: Ref[scala.Tuple2[A, B]], n: Ref[Int]): Ref[scala.Tuple2[A, B]] = Pair(PairMonoid.this.m1.power(x._1, n), PairMonoid.this.m2.power(x._2, n))
    };
    trait MonoidBuilderInstCompanion;
    trait IntPlusMonoidCompanion;
    trait IntMaxMonoidCompanion;
    trait IntMinMonoidCompanion;
    trait LongPlusMonoidCompanion;
    trait LongMaxMonoidCompanion;
    trait LongMinMonoidCompanion;
    trait PairMonoidCompanion
  }
}