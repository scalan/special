package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import spire.syntax.all._

class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  val typeGenDepth = 3
  val basicTypeGen = getBasicTypeGen(typeGenDepth)

  val testConfiguration = new GenConfiguration(maxArrayLength = 10)
  def basicValueGen[T](t: RType[T]): Gen[T] = rtypeValueGen(t, testConfiguration)

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(300)

    val typeCoverageChecker = new BasicTypeCoverageChecker()
    forAll(basicTypeGen, minSuccess) { t: RType[_] =>
      typeCoverageChecker.consider(t)
    }
    typeCoverageChecker.isFullyCovered(typeGenDepth) shouldBe true
  }

  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(100)
    forAll(basicTypeGen, minSuccess) { t: RType[_] =>
      forAll(basicValueGen(t)) { value =>
        RTypeTestUtil.valueMatchesRType(value, t)
      }
    }
  }

  property("RType clone value") {
    def checkCopy[T](value: T)(implicit tA: RType[_]): Unit = {
      var copy: T = RTypeUtil.clone(tA, value).asInstanceOf[T]

      def deepEqualityCheck[A](value: A, copy: A)(implicit tA: RType[_]): Boolean = tA match {
        case arrayType: ArrayType[a] =>
          val valInstance = value.asInstanceOf[Array[a]]
          val copyInstance = copy.asInstanceOf[Array[a]]
          valInstance.length shouldBe copyInstance.length
          cfor(0)(_ < valInstance.length, _ + 1) { i =>
            if (!deepEqualityCheck(valInstance(i), copyInstance(i))(arrayType.tA))
              return false
          }
          true
        case prim: PrimitiveType[a] =>
          copy == value
        case pairType: PairType[a, b] =>
          val valInstance = value.asInstanceOf[Tuple2[a, b]]
          val copyInstance = copy.asInstanceOf[Tuple2[a, b]]
          deepEqualityCheck(valInstance._1, copyInstance._1)(pairType.tFst) && deepEqualityCheck(valInstance._2, copyInstance._2)(pairType.tSnd)
        case (string: RType[String]@unchecked) =>
          copy == value
        case _ => copy == value
      }
      deepEqualityCheck(copy, value)(tA) shouldBe true
      val res = tA match {
        case prim: PrimitiveType[a] => true
        case _ => !(copy.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
      }
      res shouldBe true
    }

    val minSuccess = MinSuccessful(300)
    forAll(basicTypeGen, minSuccess) { tA: RType[_] =>
      forAll(basicValueGen(tA)) { value =>
        checkCopy(value.asInstanceOf[Any])(tA)
      }
    }
  }
}
