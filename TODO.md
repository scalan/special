TODO
====

#### Inheritance of type classes
```
@typeclass trait Functor[F[_]]
@typeclass trait Monad[F[_]] extends Functor[F]
```

#### Inheritance of objects
```
object Eq extends EqFunctions[Eq] with EqToEquivConversion
```

#### Implicits
- implicit arguments of SMethodDef, SClassDef
- implicit members of SEntityDef (trait, class) 
- implicit members of objects
```
object Array { 
  implicit val functorArray: Functor[Array] = ???
}
```

#### Utility method on the companion object
```
object Monoid {
  def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]
}
```

#### Preserve annotations on type arguments 
``` trait Group[@sp(Int, Long, Float, Double) A] extends Any with Monoid[A]```

#### Nested def with @tailrec
```
protected[this] def repeatedCombineN(a: A, n: Int): A = {
  @tailrec def loop(b: A, k: Int, extra: A): A = ???
}
```
#### Virtualize cats.kernels.Comparison


#### Composition if high-kinds 
```
final case class Nested[F[_], G[_], A](value: F[G[A]])
```

#### Type lambdas 
```
def compose[G[_]: Apply]: Apply[λ[α => F[G[α]]]] = new ComposedApply[F, G] 
```

#### Anonimous instantiations (typelevel/cats)
```
val x = new A { }
object Predicate {
  def apply[A](f: A => Boolean) = new Predicate[A] {
    def apply(a: A) = f(a)
  }
}
```

#### Partially-Applied Type  (typelevel/cats)
See https://tpolecat.github.io/2015/07/30/infer.html
- nested TraitDef and ClassDef
- high-kind containers like F[_,_] (at least 2 and 3)

```
object OptionT {

  private[data] final class PurePartiallyApplied[F[_]](val dummy: Boolean = true ) extends AnyVal {
    def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(Some(value)))
  }

  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]
}
```

HISTORY
=======