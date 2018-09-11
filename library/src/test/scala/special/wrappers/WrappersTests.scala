package special.wrappers

import scala.collection.mutable
import scalan.{BaseCtxTests, BaseLiftableTests}

/** Base class inhereted by test suite for each wrapper like WArray, WOption etc. */
abstract class WrappersTests extends BaseCtxTests with BaseLiftableTests {
  class WrappersCtx extends TestContext with WrappersModule with LiftableTestKit {
    import WArray._
    import Liftables._
    
    lazy val t1 = fun { (xs: Rep[WArray[Int]]) => xs.length }
    lazy val t2 = fun { (xs: Rep[WArray[Int]]) => xs(10) }
    lazy val t3 = fun { (xs: Rep[WArray[Int]]) => xs.zip(RWArray.fill(xs.length, Thunk(10))) }
    lazy val t4 = fun { (xs: Rep[WArray[Int]]) => xs.map(fun {x => x + 1}) }

  }
}
