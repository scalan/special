package scalan.primitives

import scalan.common.MetaTestsModule
import scalan.{BaseTests, Scalan}

abstract class IfThenElseTests[A <: Scalan](val ctx: A) extends BaseTests {
  import ctx._

  test("simpleIf") {
    val res = IF (true) THEN 1 ELSE 0
    res shouldEqual toRep(1)
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


class IfThenElseTestsSeq extends IfThenElseTests(new Scalan)

// Note: these tests pass thanks to rewriting of IF with constants
class IfThenElseTestsExp extends IfThenElseTests(new Scalan with MetaTestsModule) {
  import ctx._
  import MT0._; import MT1._; import MetaTest._

  test("type of if-then-else is the upper bound of its branches") {
    val c = fresh[Boolean]
    val x = IF (c) THEN RMT0(0).asRep[Any] ELSE RMT1(toRep(()), 0).asRep[Any]
    x.elem shouldEqual element[MetaTest[Unit]]
  }
}
