package special.collection

import scala.reflect.ClassTag
import scalan.OverloadId
import special.SpecialPredef._

class CostedPrim[Val](val value: Val, val cost: Int) extends Costed[Val] {
  def builder = new ConcreteCostedBuilder
}

class CostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends Costed[(L,R)] {
  def builder = new ConcreteCostedBuilder
  def value: (L,R) = (l.value, r.value)
  def cost: Int = l.cost + r.cost + 1
}

trait Closure[Env, Arg, Res] {
  def env: Env

  @OverloadId("apply_with_env")
  def apply(e: Env, a: Arg): Res
  
  @OverloadId("apply")
  def apply(a: Arg): Res = this.apply(env, a)
}

class ClosureBase[Env, Arg, Res](val env: Env, val func: ((Env, Arg)) => Res) extends Closure[Env, Arg, Res] {
  def apply(e: Env, a: Arg) = func((e, a))
}

//class ClosureCol[Env, Arg, Res](val envs: Col[Env], func: (Env, Arg) => Res) extends Col[Closure[Env, Arg, Res]] {
//}

class CostedFunc[Env,Arg,Res](envCost: Costed[Env], val func: Closure[Env, Arg, Res], val costFunc: Closure[Env,Arg,Long]) extends Costed[Arg => Res] {
  def builder = new ConcreteCostedBuilder
  def value: Arg => Res = (a: Arg) => func.apply(a)
  def cost: Int = envCost.cost + 1
}

class CostedOption[T](val either: Either[Int, Costed[T]])(implicit val cT: ClassTag[T]) extends Costed[Option[T]] {
  def builder = new ConcreteCostedBuilder
  def value: Option[T] = either.fold[Option[T]](l => none[T], r => some(r.value))
  def cost: Int = either.fold[Int](l => l, r => r.cost)
}

class CostedArray[Item](val values: Col[Item], val costs: Col[Int]) extends Costed[Array[Item]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[Item] = values.arr
  def cost: Int = costs.sum(builder.monoidBuilder.intPlusMonoid)
}

class CostedCol[Item](val values: Col[Item], val costs: Col[Int]) extends Costed[Col[Item]] {
  def builder = new ConcreteCostedBuilder
  def value: Col[Item] = values
  def cost: Int = costs.sum(builder.monoidBuilder.intPlusMonoid)
}

class CostedPairArray[L,R](val ls: Costed[Array[L]], val rs: Costed[Array[R]]) extends Costed[Array[(L,R)]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + ls.value.length
}

class CostedPairCol[L,R](val ls: Costed[Col[L]], val rs: Costed[Col[R]]) extends Costed[Col[(L,R)]] {
  def builder = new ConcreteCostedBuilder
  def value: Col[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + ls.value.length
}

class CostedNestedArray[Item](val rows: Col[Costed[Array[Item]]])(implicit val cItem: ClassTag[Item]) extends Costed[Array[Array[Item]]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[Array[Item]] = rows.map(r => r.value).arr
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
}

class CostedNestedCol[Item](val rows: Col[Costed[Col[Item]]])(implicit val cItem: ClassTag[Item]) extends Costed[Col[Col[Item]]] {
  def builder = new ConcreteCostedBuilder
  def value: Col[Col[Item]] = rows.map(r => r.value)
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
}

class ConcreteCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst
}
