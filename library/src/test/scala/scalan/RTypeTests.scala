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

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(300)

    val typeCoverageChecker = new BasicTypeCoverageChecker()
    forAll(getBasicTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      typeCoverageChecker.consider(t)
    }
    typeCoverageChecker.isFullyCovered(typeGenDepth) shouldBe true
  }
}
