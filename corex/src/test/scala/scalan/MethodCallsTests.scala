package scalan

import scala.reflect.runtime.universe._
import scalan.common.SegmentsModule
import scalan.util.ReflectionUtil

//import scalan.monads.{FreesDslExp, MonadsDslExp}

class MethodCallsTests extends BaseCtxTestsEx {
  class Ctx extends TestContextEx with SegmentsModule /*with FreesDslExp with MonadsDslExp*/ {

    /**
     * Check that calling getResultElem on TReceiver.name produces expectedResultElem.
     * params is a seq of pairs (JVM param class, param). Note that for JVM class for `Ref` is `classOf[Object]`
     */
//    def testResultElem[TReceiver, TResult](name: String, params: (Class[_], AnyRef)*)(implicit tag: TypeTag[TReceiver], expectedResultElem: Elem[TResult]) = {
//      val tpe = tag.tpe
//      val clazz = ReflectionUtil.typeTagToClass(tag)
//      val (paramClasses, realParams) = params.unzip
//      val method = clazz.getMethod(name, paramClasses: _*)
//      val receiverElem = elemFromType(tpe, Map.empty, definitions.NothingTpe)
//      val receiver = variable(Lazy(receiverElem))
//      val actualResultElem = getResultElem(receiver, method, realParams.toList)
//
//      assertResult(expectedResultElem)(actualResultElem)
//    }
  }

  ignore("getResultElem") {
    val ctx = new Ctx
    import ctx._
    import Segment._

//    testResultElem[Segment, Int]("start")
//    testResultElem[SThrowable, SThrowable]("initCause", classOf[Object] -> SThrowable(""))
    // Tests below don't work and probably need changes in testResultElem and/or getResultElem
    // pending: testResultElem[Monad[Id], Int]("unit", classOf[Object] -> toRep(0), classOf[Element[_]] -> IntElement)
    // pending: testResultElem[Array[Double], Double]("apply", classOf[Object] -> toRep(0))
  }
}
