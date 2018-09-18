package scalan.meta

import scala.reflect.ClassTag

class TypeDescTests extends BaseMetaTests {
  describe("implicit resolution") {
    it("resolve RType") {
      def test[T: RType](name: String) = {
        val t= implicitly[RType[T]]
        t.toString shouldBe name
      }
      test[Int]("WeakRType<Int>")
      test[String]("WeakRType<String>")
      test[Array[Int]]("WeakRType<int[]>")
    }

    it("resolve ClassTag") {
      import RType._
      def test[T: RType](name: String) = {
        val ct = implicitly[ClassTag[T]]
        ct.toString shouldBe name
      }
      test[Int]("Int")
      test[String]("java.lang.String")
      test[Array[Int]]("Array[int]")
    }

  }

}
