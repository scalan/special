package scalan.meta

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

class TypeDescTests extends BaseMetaTests {
  describe("implicit resolution") {
    it("resolve RType") {
      def test[T: RType](name: String) = {
        val t= implicitly[RType[T]]
        t.toString shouldBe name
      }
      test[Int]("WeakRType<Int>")(RType[Int])
      test[String]("WeakRType<String>")(RType[String])
      test[Array[Int]]("WeakRType<int[]>")(RType[Array[Int]])
    }

    it("resolve ClassTag") {
      import RType._
      def test[T: RType](name: String) = {
        val ct = implicitly[ClassTag[T]]
        ct.toString shouldBe name
      }
      test[Int]("Int")(RType[Int])
      test[String]("java.lang.String")(RType[String])
      test[Array[Int]]("Array[int]")(RType[Array[Int]])
    }

  }

}
