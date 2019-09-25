package scalan.primitives

import scalan.{Lazy, ScalanEx, BaseEx}

trait Loops extends BaseEx { self: ScalanEx =>

  def loopUntilAux[A](s1: Ref[A])(isMatch: Ref[A] => Ref[Boolean], step: Ref[A] => Ref[A]): Ref[A] = {
    val eA = s1.elem
    val leA = Lazy(eA)
    loopUntil(s1)(fun(isMatch)(leA), fun(step)(leA))
  }

  def loopUntil2[A, B](s1: Ref[A], s2: Ref[B])
                      (isMatch: (Ref[A],Ref[B]) => Ref[Boolean],
                       step: (Ref[A], Ref[B]) => (Ref[A], Ref[B])): Ref[(A,B)] = {
    loopUntilAux(Pair(s1, s2))({case Pair(a,b) => isMatch(a,b)}, {case Pair(a,b) => step(a,b)})
  }

  def loopUntil3[A, B, C](s1: Ref[A], s2: Ref[B], s3: Ref[C])
                         (isMatch: (Ref[A],Ref[B],Ref[C]) => Ref[Boolean],
                          step: (Ref[A], Ref[B], Ref[C]) => (Ref[A], Ref[B], Ref[C])): Ref[(A,(B,C))] = {
    loopUntilAux(Tuple(s1, s2, s3))({case Tuple(a,b,c) => isMatch(a,b,c) }, {case Tuple(a,b,c) => step(a,b,c)})
  }

  def loopUntil4[A, B, C, D](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D])
                            (isMatch: (Ref[A],Ref[B],Ref[C],Ref[D]) => Ref[Boolean],
                             step: (Ref[A], Ref[B], Ref[C], Ref[D]) => (Ref[A], Ref[B], Ref[C], Ref[D])): Ref[(A,(B,(C,D)))] = {
    loopUntilAux(Tuple(s1, s2, s3,s4))({case Tuple(a,b,c,d) => isMatch(a,b,c,d) }, {case Tuple(a,b,c,d) => step(a,b,c,d)})
  }

  def loopUntil5[A, B, C, D, E](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E])
                               (isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E]) => Ref[Boolean],
                                step: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E]) => (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E])): Ref[(A,(B,(C,(D,E))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5))({case Tuple(a,b,c,d,e) => isMatch(a,b,c,d,e) }, {case Tuple(a,b,c,d,e) => step(a,b,c,d,e)})

  def loopUntil6[A, B, C, D, E, F](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F])
                                  (isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F]) => Ref[Boolean],
                                   step: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F]) => (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F])): Ref[(A,(B,(C,(D,(E,F)))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5,s6))({case Tuple(a,b,c,d,e,f) => isMatch(a,b,c,d,e,f) }, {case Tuple(a,b,c,d,e,f) => step(a,b,c,d,e,f)})

  def loopUntil7[A, B, C, D, E, F, G](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7: Ref[G])
                                     (isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G]) => Ref[Boolean],
                                      step: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F],Ref[G]) => (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F],Ref[G])): Ref[(A,(B,(C,(D,(E,(F,G))))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5,s6,s7))({case Tuple(a,b,c,d,e,f,g) => isMatch(a,b,c,d,e,f,g) }, {case Tuple(a,b,c,d,e,f,g) => step(a,b,c,d,e,f,g)})

  def loopUntil8[A, B, C, D, E, F, G, H](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7: Ref[G], s8: Ref[H])
                                        (isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G],Ref[H]) => Ref[Boolean],
                                         step: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F],Ref[G],Ref[H]) => (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E],Ref[F],Ref[G],Ref[H])): Ref[(A,(B,(C,(D,(E,(F,(G,H)))))))]
  = loopUntilAux(Tuple(s1, s2, s3,s4, s5, s6, s7, s8))({case Tuple(a,b,c,d,e,f,g,h) => isMatch(a,b,c,d,e,f,g,h) }, {case Tuple(a,b,c,d,e,f,g,h) => step(a,b,c,d,e,f,g,h)})

  def from[A](s1: Ref[A]) = new From1(s1)
  def from[A, B](s1: Ref[A], s2: Ref[B]) = new From2(s1, s2)
  def from[A, B, C](s1: Ref[A], s2: Ref[B], s3: Ref[C]) =
    new From3(s1, s2, s3)
  def from[A, B, C, D](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D]) =
    new From4(s1, s2, s3, s4)
  def from[A, B, C, D, E](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E]) =
    new From5(s1, s2, s3, s4, s5)
  def from[A, B, C, D, E, F](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F]) =
    new From6(s1, s2, s3, s4, s5, s6)
  def from[A, B, C, D, E, F,G](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7:Ref[G]) =
    new From7(s1, s2, s3, s4, s5, s6, s7)
  def from[A, B, C, D, E, F, G, H](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7:Ref[G], s8:Ref[H]) =
    new From8(s1, s2, s3, s4, s5, s6, s7, s8)

  class From1[A](s1: Ref[A]) {
    def until(isMatch: Ref[A] => Ref[Boolean])(step: Ref[A] => Ref[A]) =
      loopUntilAux(s1)({case a => isMatch(a) }, {case a => step(a)})
  }
  class From2[A, B](s1: Ref[A], s2: Ref[B]) {
    def until(isMatch: (Ref[A],Ref[B]) => Ref[Boolean])(step: (Ref[A], Ref[B]) => (Ref[A], Ref[B])) =
      loopUntil2(s1, s2)(isMatch, step)
  }
  class From3[A, B, C](s1: Ref[A], s2: Ref[B], s3: Ref[C]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C]) => (Ref[A],Ref[B],Ref[C])) =
      loopUntil3(s1, s2, s3)(isMatch, step)
  }
  class From4[A, B, C, D](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C],Ref[D]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C],Ref[D]) => (Ref[A],Ref[B],Ref[C],Ref[D])) =
      loopUntil4(s1, s2, s3, s4)(isMatch, step)
  }
  class From5[A, B, C, D, E](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E]) => (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E])) =
      loopUntil5(s1, s2, s3, s4, s5)(isMatch, step)
  }
  class From6[A, B, C, D, E, F](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F]) => (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F])) =
      loopUntil6(s1, s2, s3, s4, s5,s6)(isMatch, step)
  }
  class From7[A, B, C, D, E, F,G](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7:Ref[G]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G]) => (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G])) =
      loopUntil7(s1, s2, s3, s4, s5,s6,s7)(isMatch, step)
  }
  class From8[A, B, C, D, E, F, G, H](s1: Ref[A], s2: Ref[B], s3: Ref[C], s4: Ref[D], s5: Ref[E], s6: Ref[F], s7:Ref[G], s8: Ref[H]) {
    def until(isMatch: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G],Ref[H]) => Ref[Boolean])(step: (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G],Ref[H]) => (Ref[A],Ref[B],Ref[C],Ref[D],Ref[E],Ref[F],Ref[G],Ref[H])) =
      loopUntil8(s1, s2, s3, s4, s5, s6, s7, s8)(isMatch, step)
  }

  def loopUntil[A](s1: Ref[A])(isMatch: Ref[A => Boolean], step: Ref[A => A]): Ref[A] = LoopUntil(s1, step, isMatch)

  case class LoopUntil[A](s1: Ref[A], step: Ref[A => A], isMatch: Ref[A => Boolean]) extends Def[A] {
    lazy val resultType = {
      val eState = s1.elem
      val eRange = step.elem.eRange
      assert(eState == eRange,
        s"State type of the LoopUntil should be same as step result type, but was $eState and $eRange")
      s1.elem
    }
    override def productIterator = List(step, isMatch, s1).toIterator
    override def transform(t: Transformer) = LoopUntil(t(s1), t(step), t(isMatch))
  }
}
