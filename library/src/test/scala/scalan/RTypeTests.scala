package scalan

import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import spire.syntax.all._

class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  val typeGenDepth = 5
  val coverageThreshold = 100

  val testConfiguration = new GenConfiguration(maxArrayLength = 10)
  def extendedValueGen[T](t: RType[T]): Gen[T] = rtypeValueGen(testConfiguration)(t)

  property("RType FullTypeGen coverage") {
    val minSuccess = MinSuccessful(PosInt.from(coverageThreshold).get)

    val typeCoverageChecker = new FullTypeCoverageChecker()
    forAll(extendedTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      typeCoverageChecker.consider(t)
    }
    typeCoverageChecker.isFullyCovered(typeGenDepth) shouldBe true
  }

  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessful(PosInt.from(coverageThreshold).get)
    forAll(extendedTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      forAll(extendedValueGen(t)) { value =>
        RTypeTestUtil.valueMatchesRType(value, t) shouldBe true
      }
    }
  }
}