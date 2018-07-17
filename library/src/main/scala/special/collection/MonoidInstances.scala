package special.collection {
  import scalan._

  trait MonoidInstances extends Base { self: Library =>
    import IntPlusMonoid._;
    import LongPlusMonoid._;
    import MonoidBuilder._;
    import Monoid._;
    abstract class MonoidBuilderInst extends MonoidBuilder {
      val `intPlusMonoid ` : Rep[IntPlusMonoid] = RIntPlusMonoid(toRep(0.asInstanceOf[Int]));
      def intPlusMonoid: Rep[IntPlusMonoid] = MonoidBuilderInst.this.`intPlusMonoid `;
      val `longPlusMonoid ` : Rep[LongPlusMonoid] = RLongPlusMonoid(toRep(0L.asInstanceOf[Long]));
      def longPlusMonoid: Rep[LongPlusMonoid] = MonoidBuilderInst.this.`longPlusMonoid `
    };
    abstract class IntPlusMonoid(val zero: Rep[Int]) extends Monoid[Int] {
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = x.+(y);
      def power(x: Rep[Int], n: Rep[Int]): Rep[Int] = x.*(n)
    };
    abstract class LongPlusMonoid(val zero: Rep[Long]) extends Monoid[Long] {
      def plus(x: Rep[Long], y: Rep[Long]): Rep[Long] = x.+(y);
      def power(x: Rep[Long], n: Rep[Int]): Rep[Long] = x.*(n.toLong)
    };
    trait MonoidBuilderInstCompanion;
    trait IntPlusMonoidCompanion;
    trait LongPlusMonoidCompanion
  }
}