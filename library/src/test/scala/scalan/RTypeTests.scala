package scalan

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks


class RTypeTests extends PropSpec with PropertyChecks with Matchers with RTypeGens {
  testSuite =>

  import Gen._
  import RType._

  // TODO: make normal test
  /*
  property("RType generate value by type") {
    import scala.runtime.ScalaRunTime._
    val minSuccess = MinSuccessfulprintln(30)
    forAll(fullDataTypeGen, minSuccess) { t: RType[_] =>
      forAll(rtypeValueGen(t)) { value =>

        println(stringOf(value))
      }
    }
  }

  property("RType show functionality") {
    val minSuccess = MinSuccessful(30)
    forAll(fullDataTypeGen, minSuccess) { t: RType[_] =>
      println(t)
    }
  }
  */
}