## Scala Performance Style Guide 

### Motivation

Scala is high-level language with powerful constructs and idioms to
create abstractions which allow to achieve code clarity.

However, many abstractions come with performance penalty to be payed at runtime.
Meanwhile, a given task can often be solved in many different ways. 
And it turns out that for many constructions there exists much faster alternative
which, at the same time, have comparable abstraction level and code clarity.

This document is a collection of such alternatives. The recommendations can be used
similar to a classical Style Guide, these recipes are recommendations only, there are
always exceptions.

This guide can be used by code writers and reviewers to reason about code quality from
performance point of view.
This is particularly important in those 20% of code which is known to be a hotspot or
hotspot candidate.

### Empty Seq

It is quite often to see `Seq()` where empty collection is required.
However, this means the following method from `GenericCompanion` is called

```scala
def apply[A](elems: A*): CC[A] = {
    if (elems.isEmpty) empty[A]
    else {
      val b = newBuilder[A]
      b ++= elems
      b.result()
    }
}
```

##### What to use instead

Simple `Nil` is 3-20x faster depending on the context 
(see  performance of "Seq" in `BasicBenchmarks.scala`)

### Empty Map

It is quite often to see `Map()` where empty Map is required.
However, this means the following method from `GenMapFactory` is called

```scala
def apply[A, B](elems: (A, B)*): CC[A, B] = (newBuilder[A, B] ++= elems).result()
```
##### What to use instead

Calling `Map.empty` is 50-70% faster depending on the context 
(see  performance of "Map" in `BasicBenchmarks.scala`)


### Looping using `for`

Looping pattern `for (x <- xs) { ... }` is used quite often due to it's convenience.
If looks like `x is bound to each element and block of code is executed`.
However it is desugared to `xs.foreach { x => ... }` which, besides
execution of the block of code involves the following overhead points:
1) `foreach` method call
2) allocation of lambda object
3) boxing of lambda argument for every xs item (if xs values are not yet boxed)
4) hidden overhead of concrete foreach implementation

##### What to use instead

The following code is recommended replacement if xs provides O(1) indexing operation.

```scala
import spire.syntax.all.cfor
cfor(0)(_ < xs.length, _ + 1) { i => 
  val x = xs(i)
      ...
}
```

Here `cfor` is a macros from [spire](https://github.com/non/spire) library.
This is compiled to efficient Java `for` loop and avoids overhead points 1) - 4).
Depending on xs.length it is 20-50x faster (see `BasicBenchmark.scala`)

### Sequences


### Concluding remarks

Program performance is the result of everyday work, rather than one time job.
There is no *one size fits all* solution as there are many trade-offs along the way.
You may find it useful to examine the References section for more detailed information.

### References
1. [Scala High Performance Programming](https://www.amazon.com/Scala-Performance-Programming-Vincent-Theron/dp/178646604X)
2. [Optimizing Higher-Order Functions in Scala](https://infoscience.epfl.ch/record/128135/files/paper.pdf) (somewhat outdated)
3. [Where to look first when optimizing Scala code?](https://stackoverflow.com/questions/15112604/where-to-look-first-when-optimizing-scala-code)
4. [Scala for comprehension performance](https://stackoverflow.com/questions/15137360/scala-for-comprehension-performance)
5. [Performance characteristics of Scala collections](https://docs.scala-lang.org/overviews/collections/performance-characteristics.html)
6. [Java Performance: The Definitive Guide: Getting the Most Out of Your Code](https://www.amazon.com/Java-Performance-Definitive-Guide-Getting/dp/1449358454)
7. [Scala library benchmarks](https://github.com/scala/scala/tree/2.13.x/test/benchmarks)
8. [JITWatch](https://github.com/AdoptOpenJDK/jitwatch)
9. [Parallel Collections: Measuring Performance](https://docs.scala-lang.org/overviews/parallel-collections/performance.html)
