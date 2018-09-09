package special.collection

import scala.reflect.ClassTag
import scalan.{OverloadId, NeverInline}
import special.SpecialPredef._

trait ConcreteCosted[Val] extends Costed[Val] {
  def builder = new ConcreteCostedBuilder
}

class CostedPrim[Val](val value: Val, val cost: Int, val dataSize: Long) extends ConcreteCosted[Val] {
}

class CostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends ConcreteCosted[(L,R)] {
  def value: (L,R) = (l.value, r.value)
  def cost: Int = l.cost + r.cost + builder.ConstructTupleCost
  def dataSize: Long = l.dataSize + r.dataSize
}

class CostedSum[L,R](
      val value: Either[L, R],
      val left: Costed[Unit],
      val right: Costed[Unit]) extends ConcreteCosted[Either[L, R]]
{
  @NeverInline
  def cost: Int = left.cost max right.cost + builder.ConstructSumCost
  @NeverInline
  def dataSize: Long = left.dataSize max right.dataSize + builder.SumTagSize
}

/** @param cost Cost of creating the closure object of this function, doesn't include the cost of creating environment
  * @param dataSize Size of memory necessary to store the closure object, doesn't include the dataSize of storing environment
  * */
class CostedFunc[Env,Arg,Res](
      val envCosted: Costed[Env],
      val func: Costed[Arg] => Costed[Res],
      val cost: Int,
      val dataSize: Long) extends ConcreteCosted[Arg => Res]
{
  @NeverInline
  def value: Arg => Res = ???
//  lazy val value: Arg => Res = (a: Arg) => func.apply((this.envCosted.value, a)).value
//  lazy val costFunc: Arg => Int = (a: Arg) => func.apply((this.envCosted.value, a)).cost
//  lazy val dataSizeFunc: Arg => Long = (a: Arg) => func.apply((this.envCosted.value, a)).dataSize
}

class CostedArray[Item](
      val values: Col[Item],
      val costs: Col[Int],
      val sizes: Col[Long]) extends ConcreteCosted[Array[Item]]
{
  def value: Array[Item] = values.arr
  def cost: Int = costs.sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = sizes.sum(builder.monoidBuilder.longPlusMonoid)
}

class CostedCol[Item](
      val values: Col[Item],
      val costs: Col[Int],
      val sizes: Col[Long],
      val valuesCost: Int) extends ConcreteCosted[Col[Item]]
{
  def value: Col[Item] = values
  def cost: Int = valuesCost + costs.sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = sizes.sum(builder.monoidBuilder.longPlusMonoid)
  @NeverInline
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedCol[Res] = ???
  @NeverInline
  def filterCosted(f: Costed[Item] => Costed[Boolean]): CostedCol[Item] = ???
}

class CostedPairArray[L,R](val ls: Costed[Array[L]], val rs: Costed[Array[R]]) extends ConcreteCosted[Array[(L,R)]] {
  def value: Array[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + builder.ConstructTupleCost
  def dataSize: Long = ls.dataSize + rs.dataSize
}

class CostedPairCol[L,R](val ls: Costed[Col[L]], val rs: Costed[Col[R]]) extends ConcreteCosted[Col[(L,R)]] {
  def value: Col[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + builder.ConstructTupleCost
  def dataSize: Long = ls.dataSize + rs.dataSize
}

class CostedNestedArray[Item]
      (val rows: Col[Costed[Array[Item]]])
      (implicit val cItem: ClassTag[Item]) extends ConcreteCosted[Array[Array[Item]]]
{
  def value: Array[Array[Item]] = rows.map(r => r.value).arr
  @NeverInline
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
  @NeverInline
  def dataSize: Long = rows.map(r => r.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class CostedNestedCol[Item]
      (val rows: Col[Costed[Col[Item]]])
      (implicit val cItem: ClassTag[Item]) extends ConcreteCosted[Col[Col[Item]]]
{
  def value: Col[Col[Item]] = rows.map(r => r.value)
  @NeverInline
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
  @NeverInline
  def dataSize: Long = rows.map(r => r.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class ConcreteCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst
}

//trait Closure[Env, Arg, Res] {
//  def env: Env
//
//  @OverloadId("apply_with_env")
//  def apply(e: Env, a: Arg): Res
//
//  @OverloadId("apply")
//  def apply(a: Arg): Res = this.apply(env, a)
//}
//
//class ClosureBase[Env, Arg, Res](
//    val env: Env,
//    val func: ((Env, Arg)) => Res,
//    val cost: Int,
//    val dataSize: Long) extends Closure[Env, Arg, Res]
//{
//  def apply(e: Env, a: Arg) = func((e, a))
//}

//class ClosureCol[Env, Arg, Res](val envs: Col[Env], func: (Env, Arg) => Res) extends Col[Closure[Env, Arg, Res]] {
//}

