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
      test[Int]("PrimitiveType(Int)")(RType[Int])
      test[String]("StringType")(RType[String])
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
  describe("RType") {
    def test[A](t: RType[A], n: String) = {
      t.name shouldBe n
    }
    it("has names") {
      Seq(
        BooleanType -> "Boolean",
        ByteType    -> "Byte",
        ShortType   -> "Short",
        IntType     -> "Int",
        CharType    -> "Char",
        FloatType   -> "Float",
        DoubleType  -> "Double",
        UnitType    -> "Unit",
        AnyType     -> "Any",
        AnyRefType  -> "AnyRef",
        NothingType -> "Nothing"
      ).foreach { case (t, n) => test(t, n) }
    }
  }

}
