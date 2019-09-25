/**
 * Author: Alexander Slesarenko
 * Date: 7/25/12
 */
package scalan.primitives

import scalan.OverloadHack._
import scalan.{ScalanEx, BaseEx}

trait TuplesEx extends BaseEx { self: ScalanEx =>


  implicit class TupleOps6[A,B,C,D,E,F](t: Ref[(A,(B,(C,(D,(E,F)))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Ref[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x))))) = t; x }
  }

  implicit class TupleOps7[A,B,C,D,E,F,G](t: Ref[(A,(B,(C,(D,(E,(F,G))))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Ref[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))) = t; x }
    def _7: Ref[G] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x)))))) = t; x }
  }

  implicit class TupleOps8[A,B,C,D,E,F,G,H](t: Ref[(A,(B,(C,(D,(E,(F,(G,H)))))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Ref[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))) = t; x }
    def _7: Ref[G] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))) = t; x }
    def _8: Ref[H] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x))))))) = t; x }
  }

  implicit class TupleOps9[A,B,C,D,E,F,G,H,I](t: Ref[(A,(B,(C,(D,(E,(F,(G, (H, I))))))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Ref[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))) = t; x }
    def _7: Ref[G] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))) = t; x }
    def _8: Ref[H] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))))) = t; x }
    def _9: Ref[I] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x)))))))) = t; x }
  }

  implicit class TupleOps16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](t: Ref[(A,(B,(C,(D,(E,(F,(G,(H,(I,(J,(K,(L,(M,(N,(O,P)))))))))))))))]) {
    def _1: Ref[A] = { val Pair(x, _) = t; x }
    def _2: Ref[B] = { val Pair(_, Pair(x, _)) = t; x }
    def _3: Ref[C] = { val Pair(_, Pair(_, Pair(x, _))) = t; x }
    def _4: Ref[D] = { val Pair(_, Pair(_, Pair(_, Pair(x, _)))) = t; x }
    def _5: Ref[E] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))) = t; x }
    def _6: Ref[F] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))) = t; x }
    def _7: Ref[G] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))) = t; x }
    def _8: Ref[H] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))))) = t; x }
    def _9: Ref[I] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))))) = t; x }
    def _10: Ref[J] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))))))) = t; x }
    def _11: Ref[K] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))))))) = t; x }
    def _12: Ref[L] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))))))))) = t; x }
    def _13: Ref[M] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))))))))) = t; x }
    def _14: Ref[N] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _)))))))))))))) = t; x }
    def _15: Ref[O] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(x, _))))))))))))))) = t; x }
    def _16: Ref[P] = { val Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, Pair(_, x))))))))))))))) = t; x }
  }

  implicit def zipTuple3[A, B, C](p: (Ref[A], Ref[B], Ref[C])): Ref[(A, (B, C))] =
    Tuple(p._1, p._2, p._3)

  implicit def zipTuple4[A, B, C, D](p: (Ref[A], Ref[B], Ref[C], Ref[D])): Ref[(A, (B, (C, D)))] =
    Tuple(p._1, p._2, p._3, p._4)

  implicit def zipTuple5[A, B, C, D, E](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E])): Ref[(A, (B, (C, (D, E))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5)

  implicit def zipTuple6[A, B, C, D, E, F](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E], Ref[F])): Ref[(A, (B, (C, (D, (E, F)))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5, p._6)

  implicit def zipTuple7[A, B, C, D, E, F, G](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E], Ref[F], Ref[G])): Ref[(A, (B, (C, (D, (E, (F, G))))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7)

  implicit def zipTuple8[A, B, C, D, E, F, G, H](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E], Ref[F], Ref[G], Ref[H])): Ref[(A, (B, (C, (D, (E, (F, (G, H)))))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)

  implicit def zipTuple9[A, B, C, D, E, F, G, H, I](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E], Ref[F], Ref[G], Ref[H], Ref[I])): Ref[(A, (B, (C, (D, (E, (F, (G, (H, I))))))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9)

  implicit def zipTuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](p: (Ref[A], Ref[B], Ref[C], Ref[D], Ref[E], Ref[F], Ref[G], Ref[H], Ref[I], Ref[J], Ref[K], Ref[L], Ref[M], Ref[N], Ref[O], Ref[P])): Ref[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, P)))))))))))))))] =
    Tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9, p._10, p._11, p._12, p._13, p._14, p._15, p._16)

  object Tuple {
    def apply[A, B](a: Ref[A], b: Ref[B]) = Pair(a, b)

    def apply[A, B, C](a: Ref[A], b: Ref[B], c: Ref[C]): Ref[(A, (B, C))] =
      Pair(a, Pair(b, c))

    def apply[A, B, C, D](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D]): Ref[(A, (B, (C, D)))] =
      Pair(a, Pair(b, Pair(c, d)))

    def apply[A, B, C, D, E](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E]): Ref[(A, (B, (C, (D, E))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, e))))

    def apply[A, B, C, D, E, F](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E], f: Ref[F]): Ref[(A, (B, (C, (D, (E, F)))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, Pair(e, f)))))

    def apply[A, B, C, D, E, F, G](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E], f: Ref[F], g: Ref[G]): Ref[(A, (B, (C, (D, (E, (F, G))))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, Pair(e, Pair(f, g))))))

    def apply[A, B, C, D, E, F, G, H](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E], f: Ref[F], g: Ref[G], h: Ref[H]): Ref[(A, (B, (C, (D, (E, (F, (G, H)))))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, Pair(e, Pair(f, Pair(g, h)))))))

    def apply[A, B, C, D, E, F, G, H, I](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E], f: Ref[F], g: Ref[G], h: Ref[H], i: Ref[I]): Ref[(A, (B, (C, (D, (E, (F, (G, (H, I))))))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, Pair(e, Pair(f, Pair(g, Pair(h, i))))))))

    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](a: Ref[A], b: Ref[B], c: Ref[C], d: Ref[D], e: Ref[E], f: Ref[F], g: Ref[G], h: Ref[H], i: Ref[I], j: Ref[J], k: Ref[K], l: Ref[L], m: Ref[M], n: Ref[N], o: Ref[O], p: Ref[P]): Ref[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, P)))))))))))))))] =
      Pair(a, Pair(b, Pair(c, Pair(d, Pair(e, Pair(f, Pair(g, Pair(h, Pair(i, Pair(j, Pair(k, Pair(l, Pair(m, Pair(n, Pair(o, p)))))))))))))))

    def unapply[A, B](p: Ref[(A, B)]) = Some((p._1, p._2))

    def unapply[A, B, C](p: Ref[(A, (B, C))])(implicit o: Overloaded1) =
      Some((p._1, p._2, p._3))

    def unapply[A, B, C, D](p: Ref[(A, (B, (C, D)))])(implicit o: Overloaded2) =
      Some((p._1, p._2, p._3, p._4))

    def unapply[A, B, C, D, E](p: Ref[(A, (B, (C, (D, E))))])(implicit o: Overloaded3) =
      Some((p._1, p._2, p._3, p._4, p._5))

    def unapply[A, B, C, D, E, F](p: Ref[(A, (B, (C, (D, (E, F)))))])(implicit o: Overloaded4) =
      Some((p._1, p._2, p._3, p._4, p._5, p._6))

    def unapply[A, B, C, D, E, F, G](p: Ref[(A, (B, (C, (D, (E, (F, G))))))])(implicit o: Overloaded5) =
      Some((p._1, p._2, p._3, p._4, p._5, p._6, p._7))

    def unapply[A, B, C, D, E, F, G, H](p: Ref[(A, (B, (C, (D, (E, (F, (G, H)))))))])(implicit o1: Overloaded1, o2: Overloaded1) =
      Some((p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8))

    def unapply[A, B, C, D, E, F, G, H, I](p: Ref[(A, (B, (C, (D, (E, (F, (G, (H, I))))))))])(implicit o: Overloaded6) =
      Some((p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9))

    def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](p: Ref[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, P)))))))))))))))])(implicit o: Overloaded7) =
      Some((p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8, p._9, p._10, p._11, p._12, p._13, p._14, p._15, p._16))
  }

  object TupleProjection {
    def apply[A,B](t: Ref[(A,B)], i: Int): Sym = i match {
      case 1 => t._1
      case 2 => t._2
    }
    def unapply(p: Sym): Option[Int] = p.node match {
      case First(_) => Some(1)
      case Second(_) => Some(2)
      case _ => None
    }
  }

  def projectionIndex(p: Sym): Int = p match {
    case TupleProjection(i) => i
    case _ => !!!("tuple projection expected", p)
  }

}
