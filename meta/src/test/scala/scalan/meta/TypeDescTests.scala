package scalan.meta

import scalan.RType
import scalan._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

class TypeDescTests extends BaseMetaTests {
  import RType._
  describe("implicit resolution") {
    it("resolve RType") {
      def test[T: RType](name: String) = {
        val t= RType[T]
        t.toString shouldBe name
      }
      test[Int]("ConcreteType<Int>")(RType[Int])
      test[String]("StringType$<String>")(RType[String])
    }

    it("resolve ClassTag") {
      import RType._
      def test[T: RType](name: String) = {
        val ct = implicitly[ClassTag[T]]
        ct.toString shouldBe name
      }
      test[Int]("Int")(RType[Int])
      test[String]("java.lang.String")(RType[String])
    }

  }

}
