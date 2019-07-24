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
}