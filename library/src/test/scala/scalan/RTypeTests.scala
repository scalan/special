package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import spire.syntax.all._

class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(300)

    val counter = new RTypeFullGenCoverageChecker()
    forAll(getFullTypeGen(3), minSuccess) { t: RType[_] =>
      counter.encounter(t)
    }
    counter.isCovered(3) shouldBe true
  }

  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(30)
    forAll(getFullTypeGen(3), minSuccess) { t: RType[_] =>
      forAll(rtypeValueGen(t)) { value =>
        RTypeTestUtil.valueMatchesRType(value, t)
      }
    }
  }

  property("RType clone value") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(30)
    def checkCopy[T](value: T)(implicit tA: RType[T]) = {
      val copy: T = RTypeUtil.clone(tA, value)
      copy == value shouldBe true
    }
    forAll(getFullTypeGen(3), minSuccess) { tA: RType[_] =>
      forAll(rtypeValueGen(tA), MinSuccessful(4)) { value =>
        checkCopy(value)(tA)
      }
    }
  }
}