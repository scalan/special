package special.wrappers

import scala.collection.mutable
import scalan.{BaseLiftableTests, BaseCtxTests, Library}

/** Base class inhereted by test suite for each wrapper like WArray, WOption etc. */
abstract class WrappersTests extends BaseCtxTests with BaseLiftableTests {
  class WrappersCtx extends TestContext with Library with LiftableTestKit {
  }
}
