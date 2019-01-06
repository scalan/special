package special.collection {
  import scalan._

  trait ColsOverArrays extends Base { self: Library =>
    import CReplCol._;
    import Col._;
    import ColBuilder._;
    import ColOverArray._;
    import ColOverArrayBuilder._;
    import PairCol._;
    import PairOfCols._;
    import ReplCol._;
    import WArray._;

    import Monoid._; // manual fix
    import WSpecialPredef._; // manual fix

    abstract class ColOverArray[A](val arr: Rep[WArray[A]]) extends Col[A] {
      def builder: Rep[ColBuilder] = RColOverArrayBuilder();
      def length: Rep[Int] = ColOverArray.this.arr.length;
      def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i);
      @NeverInline def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = delayInvoke;
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Col[B]] = ColOverArray.this.builder.fromArray[B](ColOverArray.this.arr.map(f));
      def foreach(f: Rep[scala.Function1[A, Unit]]): Rep[Unit] = ColOverArray.this.arr.foreach(f);
      def exists(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = ColOverArray.this.arr.exists(p);
      def forall(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = ColOverArray.this.arr.forall(p);
      def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[Col[A]] = ColOverArray.this.builder.fromArray[A](ColOverArray.this.arr.filter(p));
      @NeverInline def fold[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, A], B]]): Rep[B] = delayInvoke;
      def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = ColOverArray.this.builder.fromArray[A](ColOverArray.this.arr.slice(from, until));
      def sum(m: Rep[Monoid[A]]): Rep[A] = ColOverArray.this.arr.foldLeft(m.zero, fun(((in: Rep[scala.Tuple2[A, A]]) => {
        val b: Rep[A] = in._1;
        val a: Rep[A] = in._2;
        m.plus(b, a)
      })));
      def zip[B](ys: Rep[Col[B]]): Rep[PairCol[A, B]] = ColOverArray.this.builder.pairCol[A, B](this, ys);
      @NeverInline def append(other: Rep[Col[A]]): Rep[Col[A]] = delayInvoke
    };
    abstract class ColOverArrayBuilder extends ColBuilder {
      def pairCol[A, B](as: Rep[Col[A]], bs: Rep[Col[B]]): Rep[PairCol[A, B]] = RPairOfCols(as, bs);
      @NeverInline @Reified(value = "T") def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Col[T]] = delayInvoke;
      @NeverInline def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = delayInvoke;
      @NeverInline def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = delayInvoke;
      @NeverInline def xor(left: Rep[Col[Byte]], right: Rep[Col[Byte]]): Rep[Col[Byte]] = delayInvoke
    };
    abstract class PairOfCols[L, R](val ls: Rep[Col[L]], val rs: Rep[Col[R]]) extends PairCol[L, R] {
      override def builder: Rep[ColBuilder] = RColOverArrayBuilder();
      override def arr: Rep[WArray[scala.Tuple2[L, R]]] = PairOfCols.this.ls.arr.zip(PairOfCols.this.rs.arr);
      override def length: Rep[Int] = PairOfCols.this.ls.length;
      override def apply(i: Rep[Int]): Rep[scala.Tuple2[L, R]] = Pair(PairOfCols.this.ls.apply(i), PairOfCols.this.rs.apply(i));
      @NeverInline override def getOrElse(i: Rep[Int], default: Rep[scala.Tuple2[L, R]]): Rep[scala.Tuple2[L, R]] = delayInvoke;
      override def map[V](f: Rep[scala.Function1[scala.Tuple2[L, R], V]]): Rep[Col[V]] = RColOverArray(PairOfCols.this.arr.map(f));
      override def foreach(f: Rep[scala.Function1[scala.Tuple2[L, R], Unit]]): Rep[Unit] = PairOfCols.this.arr.foreach(f);
      override def exists(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Boolean] = PairOfCols.this.arr.exists(p);
      override def forall(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Boolean] = PairOfCols.this.arr.forall(p);
      override def filter(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Col[scala.Tuple2[L, R]]] = RColOverArray(PairOfCols.this.arr.filter(p));
      @NeverInline override def fold[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, scala.Tuple2[L, R]], B]]): Rep[B] = delayInvoke;
      override def slice(from: Rep[Int], until: Rep[Int]): Rep[PairCol[L, R]] = PairOfCols.this.builder.pairCol[L, R](PairOfCols.this.ls.slice(from, until), PairOfCols.this.rs.slice(from, until));
      def append(other: Rep[Col[scala.Tuple2[L, R]]]): Rep[Col[scala.Tuple2[L, R]]] = {
        val arrs: Rep[scala.Tuple2[Col[L], Col[R]]] = PairOfCols.this.builder.unzip[L, R](other);
        PairOfCols.this.builder.pairCol[L, R](PairOfCols.this.ls.append(arrs._1), PairOfCols.this.rs.append(arrs._2))
      };
      @NeverInline override def sum(m: Rep[Monoid[scala.Tuple2[L, R]]]): Rep[scala.Tuple2[L, R]] = delayInvoke;
      def zip[B](ys: Rep[Col[B]]): Rep[PairCol[scala.Tuple2[L, R], B]] = PairOfCols.this.builder.pairCol[scala.Tuple2[L, R], B](this, ys)
    };
    abstract class CReplCol[A](val value: Rep[A], val length: Rep[Int]) extends ReplCol[A] {
      def builder: Rep[ColBuilder] = RColOverArrayBuilder();
      def arr: Rep[WArray[A]] = CReplCol.this.builder.replicate[A](CReplCol.this.length, CReplCol.this.value).arr;
      def apply(i: Rep[Int]): Rep[A] = CReplCol.this.value;
      @NeverInline def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = delayInvoke;
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Col[B]] = RCReplCol(f.apply(CReplCol.this.value), CReplCol.this.length);
      @NeverInline def foreach(f: Rep[scala.Function1[A, Unit]]): Rep[Unit] = delayInvoke;
      def exists(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = p.apply(CReplCol.this.value);
      def forall(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = p.apply(CReplCol.this.value);
      def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[Col[A]] = IF(p.apply(CReplCol.this.value)).THEN(this).ELSE(RCReplCol(CReplCol.this.value, toRep(0.asInstanceOf[Int])));
      @NeverInline def fold[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, A], B]]): Rep[B] = delayInvoke;
      def zip[B](ys: Rep[Col[B]]): Rep[PairCol[A, B]] = CReplCol.this.builder.pairCol[A, B](this, ys);
      def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = RCReplCol(CReplCol.this.value, until.-(from));
      @NeverInline def append(other: Rep[Col[A]]): Rep[Col[A]] = delayInvoke;
      def sum(m: Rep[Monoid[A]]): Rep[A] = m.power(CReplCol.this.value, CReplCol.this.length)
    };
    trait ColOverArrayCompanion;
    trait ColOverArrayBuilderCompanion;
    trait PairOfColsCompanion;
    trait CReplColCompanion
  }
}