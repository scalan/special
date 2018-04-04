package scalan.collection {
  import scalan._

  trait ColsOverArrays extends Base { self: Library =>
    abstract class ColOverArray[A](val arr: Rep[WArray[A]]) extends Col[A] {
      def builder: Rep[ColOverArrayBuilder] = ColOverArrayBuilder();
      def length: Rep[Int] = ColOverArray.this.arr.length;
      def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i);
      def map[B](f: Rep[scala.Function1[A, B]]): Rep[Col[B]] = ColOverArray(ColOverArray.this.arr.map(f));
      def foreach(f: Rep[scala.Function1[A, Unit]]): Rep[Unit] = ColOverArray.this.arr.foreach(f)
    };
    abstract class PairOfCols[L, R](val ls: Rep[Col[L]], val rs: Rep[Col[R]]) extends PairCol[L, R] {
      def builder: Rep[ColBuilder] = ColOverArrayBuilder();
      override def arr: Rep[WArray[scala.Tuple2[L, R]]] = PairOfCols.this.ls.arr.zip(PairOfCols.this.rs.arr);
      override def length: Rep[Int] = PairOfCols.this.ls.length;
      override def apply(i: Rep[Int]): Rep[scala.Tuple2[L, R]] = Pair(PairOfCols.this.ls.apply(i), PairOfCols.this.rs.apply(i));
      override def map[V](f: Rep[scala.Function1[scala.Tuple2[L, R], V]]): Rep[Col[V]] = ColOverArray(PairOfCols.this.arr.map(f));
      def foreach(f: Rep[scala.Function1[scala.Tuple2[L, R], Unit]]): Rep[Unit] = Predef.???
    };
    abstract class ColOverArrayBuilder extends ColBuilder {
      override def apply[A, B](as: Rep[Col[A]], bs: Rep[Col[B]]): Rep[PairCol[A, B]] = PairOfCols(as, bs);
      def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = ColOverArray(arr);
      def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = ColOverArrayBuilder.this.fromArray[T](WArray.fill[T](n, Thunk(v)));
      def dot[A](xs: Rep[Col[A]], ys: Rep[Col[A]]): Rep[Nothing] = Predef.???
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