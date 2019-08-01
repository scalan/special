package scalan.primitives

import scalan.common.MetaTestsModule
import scalan.{BaseTests, Base, Scalan, DefRewriting}

abstract class IfThenElseTests[A <: Scalan](val ctx: A) extends BaseTests {
  import ctx._

  test("toRep") {
    toRep(1) shouldEqual toRep(1)
  }

  test("simpleIf") {
    val res = IF (true) THEN 1 ELSE 0
    val exp = toRep(1)
    res shouldEqual exp
  }

  test("elseIf") {
    val res = IF (false) THEN 0 ELSEIF false THEN 1 ELSE 2
    res shouldEqual toRep(2)
  }

  test("nestedElseIf") {
    val res1 = IF (false) THEN 0 ELSEIF false THEN 1 ELSEIF true THEN 2 ELSE 3
    val res2 = IF (false) THEN 0 ELSEIF false THEN 1 ELSEIF false THEN 2 ELSE 3
    res1 shouldEqual toRep(2)
    res2 shouldEqual toRep(3)
  }
}

trait IfThenElseLazyRewrites extends Base with DefRewriting { scalan: Scalan =>
  override def rewriteDef[T](d: Def[T]): Rep[_] = d match {
    // Rule: if (true) t else e ==> t
    case IfThenElseLazy(Def(Const(true)), t, _) => reifyObject(ThunkForce(t))

    // Rule: if (false) t else e ==> e
    case IfThenElseLazy(Def(Const(false)), _, e) => reifyObject(ThunkForce(e))
    case _ =>
      super.rewriteDef(d)
  }
}
class IfThenElseTestsSeq extends IfThenElseTests(new Scalan with IfThenElseLazyRewrites)

// Note: these tests pass thanks to rewriting of IF with constants
class IfThenElseTestsExp extends IfThenElseTests(ctx = new Scalan with MetaTestsModule with IfThenElseLazyRewrites) {
  import ctx._
  import MT0._; import MT1._; import MetaTest._

  // TODO implement (this was removed when Elem.leastUpperBound was removed)
  ignore("type of if-then-else is the upper bound of its branches") {
    val c = variable[Boolean]
    val x = IF (c) THEN asRep[Any](RMT0(0)) ELSE asRep[Any](RMT1(toRep(()), 0))
    x.elem shouldEqual element[MetaTest[Unit]]
  }
}
