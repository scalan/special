package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import spire.syntax.all._

class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  val typeGenerationDepth = 3

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(300)

    val counter = new RTypeFullGenCoverageChecker()
    forAll(getFullTypeGen(typeGenerationDepth), minSuccess) { t: RType[_] =>
      counter.encounter(t)
    }
    counter.isCovered(typeGenerationDepth) shouldBe true
  }

  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(100)
    forAll(getFullTypeGen(typeGenerationDepth), minSuccess) { t: RType[_] =>
      forAll(rtypeValueGen(t)) { value =>
        RTypeTestUtil.valueMatchesRType(value, t)
      }
    }
  }

  property("RType clone value") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(30)
    def checkCopy[T](value: T)(implicit tA: RType[_]): Unit = {
      var copy: T = RTypeUtil.clone(tA, value).asInstanceOf[T]

      def deepEqualityChecker[A](value: A, copy: A)(implicit tA: RType[_]): Boolean = tA match {
        case arrayType: ArrayType[a] =>
          val valInstance = value.asInstanceOf[Array[a]]
          val copyInstance = copy.asInstanceOf[Array[a]]
          valInstance.length shouldBe copyInstance.length
          cfor(0)(_ < valInstance.length, _ + 1) { i =>
            if (!deepEqualityChecker(valInstance(i), copyInstance(i))(arrayType.tA))
              return false
          }
          true
        case prim: PrimitiveType[a] =>
          copy == value
        case pairType: PairType[a, b] =>
          val valInstance = value.asInstanceOf[Tuple2[a, b]]
          val copyInstance = copy.asInstanceOf[Tuple2[a, b]]
          deepEqualityChecker(valInstance._1, copyInstance._1)(pairType.tFst) && deepEqualityChecker(valInstance._2, copyInstance._2)(pairType.tSnd)
        case (string: RType[String]@unchecked) =>
          copy == value
        case _ => copy == value
      }
      deepEqualityChecker(copy, value)(tA) shouldBe true
      val res = tA match {
        case prim: PrimitiveType[a] => true
        case _ => !(copy.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
      }
      res shouldBe true
    }

    forAll(getFullTypeGen(typeGenerationDepth), MinSuccessful(300)) { tA: RType[_] =>
      forAll(rtypeValueGen(tA)) { value =>
        checkCopy(value.asInstanceOf[Any])(tA)
      }
    }
  }
}