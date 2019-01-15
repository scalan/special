package special.collection

import scala.reflect.ClassTag
import scalan.{NeverInline, Internal, Reified, OverloadId}
import special.SpecialPredef._
import scalan.meta.RType

class CCostedPrim[Val](val value: Val, val cost: Int, val dataSize: Long) extends CostedPrim[Val] {
  def builder: CostedBuilder = new CCostedBuilder
}

class CCostedPair[L,R](val l: Costed[L], val r: Costed[R]) extends CostedPair[L,R] {
  def builder: CostedBuilder = new CCostedBuilder
  def value: (L,R) = (l.value, r.value)
  def cost: Int = l.cost + r.cost + builder.ConstructTupleCost
  def dataSize: Long = l.dataSize + r.dataSize
}

class CCostedSum[L,R](
      val value: Either[L, R],
      val left: Costed[Unit],
      val right: Costed[Unit]) extends CostedSum[L, R]
{
  def builder: CostedBuilder = new CCostedBuilder
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
  def builder: CostedBuilder = new CCostedBuilder
  @NeverInline def value: Arg => Res = rewritableMethod
}

class CCostedCol[Item](
      val values: Coll[Item],
      val costs: Coll[Int],
      val sizes: Coll[Long],
      val valuesCost: Int) extends CostedCol[Item]
{
  def builder: CostedBuilder = new CCostedBuilder
  def value: Coll[Item] = values
  def cost: Int = valuesCost + costs.sum(builder.monoidBuilder.intPlusMonoid)
  def dataSize: Long = sizes.sum(builder.monoidBuilder.longPlusMonoid)
  @NeverInline
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedCol[Res] = rewritableMethod
  @NeverInline
  def filterCosted(f: Costed[Item] => Costed[Boolean]): CostedCol[Item] = rewritableMethod
  @NeverInline
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]): Costed[B] = rewritableMethod
}

class CCostedPairCol[L,R](val ls: Costed[Coll[L]], val rs: Costed[Coll[R]]) extends CostedPairCol[L,R] {
  def builder: CostedBuilder = new CCostedBuilder
  def value: Coll[(L,R)] = ls.value.zip(rs.value)
  def cost: Int = ls.cost + rs.cost + builder.ConstructTupleCost
  def dataSize: Long = ls.dataSize + rs.dataSize
}

class CCostedNestedCol[Item]
      (val rows: Coll[Costed[Coll[Item]]])
      (implicit val cItem: ClassTag[Item]) extends CostedNestedCol[Item]
{
  def builder: CostedBuilder = new CCostedBuilder
  @NeverInline
  def value: Coll[Coll[Item]] = rows.map(r => r.value)
  @NeverInline
  def cost: Int = rows.map(r => r.cost).sum(builder.monoidBuilder.intPlusMonoid)
  @NeverInline
  def dataSize: Long = rows.map(r => r.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class CCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst

  @NeverInline
  def costedValue[T](x: T, optCost: Option[Int])(implicit cT: RType[T]): Costed[T] = rewritableMethod

  @NeverInline
  def defaultValue[T](valueType: RType[T]): T = rewritableMethod

  def mkCostedPrim[T](value: T, cost: Int, size: Long): CostedPrim[T] =
    new CCostedPrim[T](value, cost, size)

  def mkCostedPair[L,R](first: Costed[L], second: Costed[R]): CostedPair[L,R] =
    new CCostedPair(first, second)

  def mkCostedSum[L,R](value: Either[L, R], left: Costed[Unit], right: Costed[Unit]): CostedSum[L, R] =
    new CCostedSum(value, left, right)

  def mkCostedFunc[Env,Arg,Res](
        envCosted: Costed[Env],
        func: Costed[Arg] => Costed[Res],
        cost: Int, dataSize: Long): CostedFunc[Env, Arg, Res] = new CCostedFunc(envCosted, func, cost, dataSize)

  def mkCostedCol[T](values: Coll[T], costs: Coll[Int], sizes: Coll[Long], valuesCost: Int): CostedCol[T] =
    new CCostedCol[T](values, costs, sizes, valuesCost)

  def mkCostedPairCol[L,R](ls: Costed[Coll[L]], rs: Costed[Coll[R]]): CostedPairCol[L,R] =
    new CCostedPairCol(ls, rs)

  def mkCostedNestedCol[Item](rows: Coll[Costed[Coll[Item]]])(implicit cItem: ClassTag[Item]): CostedNestedCol[Item] =
    new CCostedNestedCol[Item](rows)

  def mkCostedSome[T](costedValue: Costed[T]): CostedOption[T] =
    new CostedSome[T](costedValue)

  def mkCostedNone[T](cost: Int)(implicit eT: RType[T]): CostedOption[T] =
    new CostedNone[T](cost)

  def mkCostedOption[T](value: Option[T], costOpt: Option[Int], sizeOpt: Option[Long], accumulatedCost: Int): CostedOption[T] =
    new CCostedOption[T](value, costOpt, sizeOpt, accumulatedCost)
}


