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

  property("RType clone value") {
    import scala.runtime.ScalaRunTime._
    def checkCopy[T](value: T, tA: RType[_]): Unit = {
      var copy: T = RTypeUtil.clone(tA, value).asInstanceOf[T]
      println(stringOf(value))
      println(stringOf(copy))

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
        case StringType =>
          copy == value
        case _ => copy == value
      }
      deepEqualityChecker(copy, value)(tA) shouldBe true
      val res = tA match {
        case prim: PrimitiveType[a] => true
        case optionType: OptionType[a] if value.asInstanceOf[Option[a]].isEmpty => true
        case _ => !(copy.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
      }
      res shouldBe true
    }

    val minSuccess = MinSuccessful(PosInt.from(coverageThreshold).get)
    forAll(extendedTypeGen(typeGenDepth), minSuccess) { t: RType[_] =>
      forAll(extendedValueGen(t)) { value =>
        println(t)
        checkCopy(value, t)
      }
    }
  }
}
