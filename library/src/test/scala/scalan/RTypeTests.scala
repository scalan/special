package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks


class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  /*
  property("RType show functionality") {
    val minSuccess = MinSuccessful(30)
    forAll(fullDataTypeGen, minSuccess) { t: RType[_] =>
      println(t)
    }
  }
  */
}