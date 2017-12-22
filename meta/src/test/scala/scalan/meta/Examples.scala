package scalan.meta

trait Examples { self: BaseMetaTests =>

  val reactiveModule = TestModule("Reactives",
    """package scalan.rx
     |import scalan._
     |trait Reactives extends Scalan {
     |  type Obs[A] = Rep[Observable[A]]
     |  trait Observable[A] {
     |    implicit def eA: Elem[A]
     |  }
     |  class ObservableImpl1[A](implicit val eA: Elem[A]) extends Observable[A] {
     |  }
     |  class ObservableImpl2[A](implicit val eA: Elem[A]) extends Observable[A] {
     |  }
     |}
    """.stripMargin, true)

  val colsModule = TestModule("Cols",
    """package scalan.collection
     |  class ColOverArray[A](val arr: Array[A]) extends Collection[A] {
     |    val list: List[A] = arr.toList
     |    def length: Int = ColOverArray.this.arr.length;
     |    def apply(i: Int): A = ColOverArray.this.arr.apply(i)
     |  };
     |  trait PairCollection[L, R] extends Collection[(L,R)]{
     |    def ls: Col[L]
     |    def rs: Col[R]
     |  }
     |  trait Collection[A] {
     |    def length: Int;
     |    def apply(i: Int): A
     |  };
     |  class PairOfCols[L,R](val ls: Col[L], val rs: Col[R]) extends PairCollection[L,R] {
     |    override def length: Int = ls.length
     |    override def apply(i: Int): (L, R) = (ls(i), rs(i))
     |  }
    """.stripMargin, false)

  val colsVirtModule = TestModule("Cols",
    """package scalan.collection
     |import scalan._
     |trait Cols extends Scalan {
     |  type Col[A] = Rep[Collection[A]]
     |  abstract class ColOverArray[A](val arr: Rep[WArray[A]])(implicit val eA: Elem[A]) extends Collection[A] {
     |    val list: Rep[WList[A]] = arr.toList
     |    def length: Rep[Int] = ColOverArray.this.arr.length;
     |    def apply(i: Rep[Int]): Rep[A] = ColOverArray.this.arr.apply(i)
     |  };
     |  trait PairCollection[L, R] extends Collection[(L,R)]{
     |    implicit def eL: Elem[L];
     |    implicit def eR: Elem[R];
     |    def ls: Rep[Collection[L]];
     |    def rs: Rep[Collection[R]]
     |  }
     |  trait Collection[A] extends Def[Collection[A]] {
     |    implicit def eA: Elem[A]
     |    def arr: Rep[WArray[A]];
     |    def length: Rep[Int];
     |    def apply(i: Rep[Int]): Rep[A]
     |  }
     |  abstract class PairOfCols[L, R](val ls: Rep[Collection[L]], val rs: Rep[Collection[R]])
     |                                 (implicit val eL: Elem[L], val eR: Elem[R]) extends PairCollection[L, R] {
     |    override def length: Rep[Int] = PairOfCols.this.ls.length;
     |    override def apply(i: Rep[Int]): Rep[scala.Tuple2[L, R]] = Pair(PairOfCols.this.ls.apply(i), PairOfCols.this.rs.apply(i));
     |  };
     |}
    """.stripMargin, true)

  val itersApiModule = TestModule("ItersApi",
    """package scalan.iter
     |import scalan._
     |trait Iter[A] {
     |  def length: Int;
     |  def apply(i: Int): A
     |}
     |trait IterBuilder {
     |  def fromArray[T](arr: Array[T]): Iter[T]
     |}
    """.stripMargin, false)

  val itersImplModule = TestModule("ItersImpl",
    """package scalan.iter
     |import scalan._
     |class IterOverArray[A](val arr: Array[A]) extends Iter[A] {
     |  val list: List[A] = arr.toList
     |  def length = arr.length
     |  def apply(i: Int) = arr(i)
     |}
     |class IterOverArrayBuilder extends IterBuilder {
     |  def fromArray[T](arr: Array[T]): Iter[T] = new IterOverArray[T](arr)
     |}
    """.stripMargin, false)

  val warraysModule = TestModule("WArrays",
    """
     |package scala {
     |  import scalan._
     |  import scala.wrappers.WrappersModule
     |  import impl._
     |
     |  trait WArrays extends Base { self: WrappersModule =>
     |    type RepWArray[T] = Rep[WArray[T]];
     |    @External("Array") @ContainerType @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
     |      implicit def eT: Elem[T];
     |      @External def apply(i: Rep[Int]): Rep[T];
     |      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
     |      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
     |      @External def length: Rep[Int]
     |    };
     |    trait WArrayCompanion {
     |      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
     |    }
     |  }
     |}
    """.stripMargin, true)

  val warrays1Module = TestModule("WArrays",
    """
     |package scala {
     |  import scalan._
     |  import scala.wrappers.WrappersModule
     |
     |  trait WArrays extends Base { self: WrappersModule =>
     |    type RepWArray[T] = Rep[WArray[T]];
     |    @External("Array") @ContainerType trait WArray[T] extends Def[WArray[T]] { self =>
     |      implicit def eT: Elem[T];
     |      @External def apply(i: Rep[Int]): Rep[T];
     |      @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]];
     |    };
     |    trait WArrayCompanion {
     |    }
     |  }
     |}
    """.stripMargin, true)

  val warrays2Module = TestModule("WArrays",
    """
     |package scala {
     |  import scalan._
     |  import impl._
     |
     |  trait WArrays extends Base { self: WrappersModule =>
     |    type RepWArray[T] = Rep[WArray[T]];
     |    @External("Array") @FunctorType trait WArray[T] extends Def[WArray[T]] { self =>
     |      @External def apply(i: Rep[Int]): Rep[T];
     |      @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
     |      @External def length: Rep[Int]
     |    };
     |    trait WArrayCompanion {
     |      @External def fill[@Reified T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]]
     |    }
     |  }
     |}
    """.stripMargin, true)

}
