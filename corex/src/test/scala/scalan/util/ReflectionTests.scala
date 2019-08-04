package scalan.util

import scala.reflect.runtime.universe._
import scalan.common.{SegmentsModule}
import scalan.{Scalan, BaseCtxTests}

class ReflectionTests extends BaseCtxTests {

  trait ReflectionExamples extends Scalan with SegmentsModule {
  }

}