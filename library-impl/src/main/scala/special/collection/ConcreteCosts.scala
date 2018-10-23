package special.collection

import scala.reflect.ClassTag
import scalan.{NeverInline, Internal, Reified, OverloadId}
import special.SpecialPredef._

import scalan.meta.RType

//trait ConcreteCosted[Val] extends Costed[Val] {
//  def builder = new ConcreteCostedBuilder
//}


class CCostedPrim[Val](val value: Val, val cost: Int, val dataSize: Long) extends CostedPrim[Val] {
  def builder: CostedBuilder = new ConcreteCostedBuilder
}

class CCostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends CostedPair[L,R] {
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: (L,R) = (l.value, r.value)
  def cost: Int = l.cost + r.cost + builder.ConstructTupleCost
  def dataSize: Long = l.dataSize + r.dataSize
}

class CCostedSum[L,R](
      val value: Either[L, R],
      val left: Costed[Unit],
      val right: Costed[Unit]) extends CostedSum[L, R]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  @NeverInline
  def cost: Int = left.cost max right.cost + builder.ConstructSumCost
  @NeverInline
  def dataSize: Long = left.dataSize max right.dataSize + builder.SumTagSize
}

/** @param cost Cost of creating the closure object of this function, doesn't include the cost of creating environment
  * @param dataSize Size of memory necessary to store the closure object, doesn't include the dataSize of storing environment
  * */
class CCostedFunc[Env,Arg,Res](
      val envCosted: Costed[Env],
      val func: Costed[Arg] => Costed[Res],
      val cost: Int,
      val dataSize: Long) extends CostedFunc[Env, Arg, Res]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  @NeverInline
  def value: Arg => Res = ???
//  lazy val value: Arg => Res = (a: Arg) => func.apply((this.envCosted.value, a)).value
//  lazy val costFunc: Arg => Int = (a: Arg) => func.apply((this.envCosted.value, a)).cost
//  lazy val dataSizeFunc: Arg => Long = (a: Arg) => func.apply((this.envCosted.value, a)).dataSize
}

class CCostedArray[Item](
      val values: Col[Item],
      val costs: Col[Int],
      val sizes: Col[Long]) extends CostedArray[Item]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Array[Item] = values.arr
  def cost: Int = costs.sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = sizes.sum(builder.monoidBuilder.longPlusMonoid)
}

class CCostedCol[Item](
      val values: Col[Item],
      val costs: Col[Int],
      val sizes: Col[Long],
      val valuesCost: Int) extends CostedCol[Item]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Col[Item] = values
  def cost: Int = valuesCost + costs.sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = sizes.sum(builder.monoidBuilder.longPlusMonoid)
  @NeverInline
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedCol[Res] = rewritableMethod
  @NeverInline
  def filterCosted(f: Costed[Item] => Costed[Boolean]): CostedCol[Item] = rewritableMethod
  @NeverInline
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]) = rewritableMethod
}

class CCostedPairArray[L,R](val ls: Costed[Array[L]], val rs: Costed[Array[R]]) extends CostedPairArray[L,R] {
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Array[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + builder.ConstructTupleCost
  def dataSize: Long = ls.dataSize + rs.dataSize
}

class CCostedPairCol[L,R](val ls: Costed[Col[L]], val rs: Costed[Col[R]]) extends CostedPairCol[L,R] {
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Col[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + builder.ConstructTupleCost
  def dataSize: Long = ls.dataSize + rs.dataSize
}


class CCostedNestedArray[Item]
      (val rows: Col[Costed[Array[Item]]])
      (implicit val cItem: ClassTag[Item]) extends CostedNestedArray[Item]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Array[Array[Item]] = rows.map(r => r.value).arr
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = rows.map(r => r.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class CCostedNestedCol[Item]
      (val rows: Col[Costed[Col[Item]]])
      (implicit val cItem: ClassTag[Item]) extends CostedNestedCol[Item]
{
  def builder: CostedBuilder = new ConcreteCostedBuilder
  def value: Col[Col[Item]] = rows.map(r => r.value)
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = rows.map(r => r.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class ConcreteCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst

  @NeverInline
  def costedValue[T](x: T, optCost: Option[Int])(implicit cT: RType[T]): Costed[T] = rewritableMethod

  @NeverInline
  def defaultValue[T](valueType: RType[T]): T = rewritableMethod
}

