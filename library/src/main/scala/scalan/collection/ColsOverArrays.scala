package scalan.collection {
  import scalan._

  trait ColsOverArrays extends Base { self: Library =>
    abstract class ColOverArray[A](val arr: Rep[WArray[A]]) extends Col[A] {
      def builder: Rep[ColOverArrayBuilder] = ColOverArrayBuilder();
      def length: Rep[Int] = ColOverArray.this.arr.length;
      def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i);
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Col[B]] = ColOverArray.this.builder.fromArray[B](ColOverArray.this.arr.map(f));
      def foreach(f: Rep[scala.Function1[A, Unit]]): Rep[Unit] = ColOverArray.this.arr.foreach(f);
      def exists(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = ColOverArray.this.arr.exists(p);
      def forall(p: Rep[scala.Function1[A, Boolean]]): Rep[Boolean] = ColOverArray.this.arr.forall(p);
      def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[Col[A]] = ColOverArray.this.builder.fromArray[A](ColOverArray.this.arr.filter(p));
      def fold[B](zero: Rep[B])(op: Rep[scala.Function1[scala.Tuple2[B, A], B]]): Rep[B] = ColOverArray.this.arr.foldLeft(zero, op);
      def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = ColOverArray.this.builder.fromArray[A](ColOverArray.this.arr.slice(from, until))
    };
    abstract class PairOfCols[L, R](val ls: Rep[Col[L]], val rs: Rep[Col[R]]) extends PairCol[L, R] {
      override def builder: Rep[ColBuilder] = ColOverArrayBuilder();
      override def arr: Rep[WArray[scala.Tuple2[L, R]]] = PairOfCols.this.ls.arr.zip(PairOfCols.this.rs.arr);
      override def length: Rep[Int] = PairOfCols.this.ls.length;
      override def apply(i: Rep[Int]): Rep[scala.Tuple2[L, R]] = Pair(PairOfCols.this.ls.apply(i), PairOfCols.this.rs.apply(i));
      override def map[V](f: Rep[scala.Function1[scala.Tuple2[L, R], V]]): Rep[Col[V]] = ColOverArray(PairOfCols.this.arr.map(f));
      override def foreach(f: Rep[scala.Function1[scala.Tuple2[L, R], Unit]]): Rep[Unit] = PairOfCols.this.arr.foreach(f);
      override def exists(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Boolean] = PairOfCols.this.arr.exists(p);
      override def forall(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Boolean] = PairOfCols.this.arr.forall(p);
      override def filter(p: Rep[scala.Function1[scala.Tuple2[L, R], Boolean]]): Rep[Col[scala.Tuple2[L, R]]] = ColOverArray(PairOfCols.this.arr.filter(p));
      override def fold[B](zero: Rep[B])(op: Rep[scala.Function1[scala.Tuple2[B, scala.Tuple2[L, R]], B]]): Rep[B] = PairOfCols.this.arr.foldLeft(zero, op);
      override def slice(from: Rep[Int], until: Rep[Int]): Rep[PairCol[L, R]] = PairOfCols.this.builder.apply[L, R](PairOfCols.this.ls.slice(from, until), PairOfCols.this.rs.slice(from, until))
    };
    abstract class ColOverArrayBuilder extends ColBuilder {
      @OverloadId(value = "apply") def apply[A, B](as: Rep[Col[A]], bs: Rep[Col[B]]): Rep[PairCol[A, B]] = PairOfCols(as, bs);
      @OverloadId(value = "apply_items") def apply[T](items: Rep[T]*): Rep[Col[T]] = Predef.???;
      def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = ColOverArray(arr);
      def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = ColOverArrayBuilder.this.fromArray[T](WArray.fill[T](n, Thunk(v)));
      def dot[A](xs: Rep[Col[A]], ys: Rep[Col[A]]): Rep[A] = Predef.???
    };
    abstract class ArrayFunctor extends Functor[WArray] {
      override def map[A, B](fa: Rep[WArray[A]])(f: Rep[scala.Function1[A, B]]): Rep[WArray[B]] = fa.map(f)
    };
    trait ColOverArrayCompanion;
    trait PairOfColsCompanion;
    trait ColOverArrayBuilderCompanion;
    trait ArrayFunctorCompanion
  }
}