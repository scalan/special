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
      test(RType[(Int, Long)], "(Int, Long)")
      test(RType[Option[(Int, Long)]], "Option[(Int, Long)]")
      test(RType[Array[(Int, Long)]], "Array[(Int, Long)]")
      test(RType[Either[Int, Long]], "(Int | Long)")
      test(tupleRType(Array(IntType, LongType, RType[(String, Double)], RType[Option[Boolean]])),
           "(Int, Long, (String, Double), Option[Boolean])")
    }

    it("implements equality") {
      def test[A: RType, B: RType] = {
        val x = RType[A]; val y = RType[B]
        assert(x == y)
      }
      test[Boolean, Boolean]
      test[Byte, Byte]
      test[Short, Short]
      test[Int, Int]
      test[Long, Long]
      test[Char, Char]
      test[Float, Float]
      test[Double, Double]
      test[Unit, Unit]
      test[String, String]
      test[(Int, Long), (Int, Long)]
      test[Array[(Int, Long)], Array[(Int, Long)]]
      
      assert(pairRType[Int, Long] == pairRType[Int, Long])
      assert(pairRType[Int, Long] != pairRType[Long, Int])

      def tuple = tupleRType(Array(RType[Int], RType[Long]))
      assert(tuple == tuple, "compare two different but equal instances")

      def tuple2 = tupleRType(Array(RType[Long], RType[Int]))
      assert(tuple != tuple2, "compare two different types")

      def struct = structRType(Array("x", "y"), Array(RType[Int], RType[Long]))
      assert(struct == struct, "compare two different but equal instances")

      def struct2 = structRType(Array("x", "y2"), Array(RType[Int], RType[Long]))
      assert(struct != struct2, "changed single field name")

      def struct3 = structRType(Array("x", "y"), Array(RType[Int], RType[Int]))
      assert(struct != struct3, "changed single field type")
    }
  }

}
