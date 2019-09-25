package scalan.primitives

import scalan.Scalan

trait PrimitiveExamples extends Scalan {
  lazy val id = fun {(x: Ref[Int]) => x}
  lazy val inc = fun {(x: Ref[Int]) => x + 1}
  lazy val curred = fun {(x: Ref[Int]) => fun { (y: Ref[Int]) => x + y }}
  lazy val tupled = fun {(x: Ref[(Int,Int)]) => x._1 + x._2 }
  lazy val highOrder = fun {(x: Ref[Int]) => {
    val x1 = x + 1
    fun {(y: Ref[Int]) => y + x + x1 }
  } }
  lazy val inc2 = fun {(x: Ref[Int]) => x + ((1:Ref[Int]) + 1)}
  lazy val inc_times = fun {(x: Ref[Int]) => x + ((1:Ref[Int]) + 1) * 2 }
  lazy val scalar = fun {(x: Ref[Int]) => (x + 1) * (x + 2) }

  lazy val ifsWithCommonCond = fun { a: Ref[Int] =>
    val c = a * 2
    val if1 = IF (a < 10) THEN { c } ELSE {a - 1}
    val if2 = IF (a < 10) THEN {c + 1} ELSE {a - 1}

    if1 + if2
  }

}
