package scalan.collection

import scala.reflect.ClassTag
import scalan.OverloadId

class CostedPrim[Val](val value: Val, val cost: Long) extends Costed[Val] {
  def builder = new ConcreteCostedBuilder
}

class CostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends Costed[(L,R)] {
  def builder = new ConcreteCostedBuilder
  def value: (L,R) = (l.value, r.value)
  def cost: Long = l.cost + r.cost + 1
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
  def cost: Long = envCost.cost + 1L
}

//class CostedOption[T](val either: Either[Long, Costed[T]]) extends Costed[Option[T]] {
//  def value: Option[T] = either.fold(l => None, r => r.value)
//  def cost = either.fold(l => l, r => r.cost)
//}

class CostedArray[Item](val values: Col[Item], val costs: Col[Long]) extends Costed[Array[Item]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[Item] = values.arr
  def cost: Long = costs.sum(builder.monoidBuilder.longPlusMonoid)
}

class CostedPairArray[L,R](val ls: Costed[Array[L]], val rs: Costed[Array[R]]) extends Costed[Array[(L,R)]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[(L,R)] = ls.value.zip(rs.value)
  def cost: Long = ls.cost + rs.cost + ls.value.length
}

class CostedNestedArray[Item](val rows: Col[Costed[Array[Item]]])(implicit val cItem: ClassTag[Item]) extends Costed[Array[Array[Item]]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[Array[Item]] = rows.map(r => r.value).arr
  def cost: Long = rows.map(r => r.cost).sum(builder.monoidBuilder.longPlusMonoid)
}

class ConcreteCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst
}
