package special.collection

import special.SpecialPredef

import scalan.meta.RType
import RType._
import special.SpecialPredef._

import scalan.NeverInline

class CostedSome[T](val costedValue: Costed[T]) extends CostedOption[T] {
  def builder: CostedBuilder = new CCostedBuilder
  def value: Option[T] = SpecialPredef.some(costedValue.value)
  def dataSize: Long = builder.SumTagSize + costedValue.dataSize
  def cost: Int = costedValue.cost + builder.ConstructSumCost  // see doc comments for CostedOption
  def get: Costed[T] = costedValue
  def getOrElse(default: Costed[T]): Costed[T] = costedValue

  @NeverInline
  def fold[B](ifEmpty: Costed[B], f: Costed[T => B]): Costed[B] = rewritableMethod

  def isEmpty: Costed[Boolean] = new CCostedPrim(false, costedValue.cost + builder.SelectFieldCost, 1L)

  def isDefined: Costed[Boolean] = new CCostedPrim(true, costedValue.cost + builder.SelectFieldCost, 1L)

  @NeverInline
  def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod

  @NeverInline
  def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod

  @NeverInline
  def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}

class CostedNone[T](val cost: Int)(implicit val eT: RType[T]) extends CostedOption[T] {
  def builder: CostedBuilder = new CCostedBuilder
  def value: Option[T] = SpecialPredef.none[T](eT)
  def dataSize: Long = builder.SumTagSize
  def get: Costed[T] = builder.costedValue(builder.defaultValue(eT), SpecialPredef.some(cost))

  @NeverInline
  def getOrElse(default: Costed[T]): Costed[T] = rewritableMethod

  @NeverInline
  def fold[B](ifEmpty: Costed[B], f: Costed[T => B]): Costed[B] = rewritableMethod

  def isEmpty: Costed[Boolean] = new CCostedPrim(true, cost + builder.SelectFieldCost, 1L)

  def isDefined: Costed[Boolean] = new CCostedPrim(false, cost + builder.SelectFieldCost, 1L)

  @NeverInline
  def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod

  @NeverInline
  def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod

  @NeverInline
  def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}

/** CostedOption is represented similar to CostedCol, because option can be represented as collection of 0 or 1 elements.
  * @param value optional value
  * @param costOpt optional cost of producing the optional value
  * @param sizeOpt optional size of the optional value
  * @param accumulatedCost accumulated cost to produce this option object (but not the cost of producing the value it may cost) */
class CCostedOption[T](
    val value: Option[T],
    val costOpt: Option[Int],
    val sizeOpt: Option[Long],
    val accumulatedCost: Int
    ) extends CostedOption[T]
{
  def builder: CostedBuilder = new CCostedBuilder
  def cost: Int = accumulatedCost + costOpt.getOrElse(0)
  def dataSize: Long = sizeOpt.getOrElse(0L)

  def get: Costed[T] = builder.mkCostedPrim(value.get, cost, dataSize)
  def getOrElse(default: Costed[T]): Costed[T] = {
    val v = value.getOrElse(default.value)
    val c = accumulatedCost + costOpt.getOrElse(default.cost)
    val s = sizeOpt.getOrElse(default.dataSize)
    builder.mkCostedPrim(v, c, s)
  }
  def isEmpty: Costed[Boolean] = builder.mkCostedPrim(value.isEmpty, cost, 1L)
  def isDefined: Costed[Boolean] = builder.mkCostedPrim(value.isDefined, cost, 1L)
  @NeverInline def fold[B](ifEmpty: Costed[B], f: Costed[T => B]): Costed[B] = rewritableMethod
  @NeverInline def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod
  @NeverInline def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod
  @NeverInline def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}

