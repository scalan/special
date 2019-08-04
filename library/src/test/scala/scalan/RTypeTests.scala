package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import spire.syntax.all._

class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  val typeGenDepth = 10

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(300)

    val typeCoverageChecker = new FullTypeCoverageChecker()
    forAll(extendedTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      typeCoverageChecker.consider(t)
    }
    typeCoverageChecker.isFullyCovered(typeGenDepth) shouldBe true
  }

  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(10)
    forAll(extendedTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      println(t)
      forAll(rtypeValueGen(t)) { value =>
        RTypeTestUtil.valueMatchesRType(value, t)
      }
    }
  }
}
