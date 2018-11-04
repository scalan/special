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

class CCostedOption[T](
    val value: Option[T],
    val none: Costed[Unit],
    val some: Costed[Unit]) extends CostedOption[T]
{
  def builder: CostedBuilder = new CCostedBuilder
  @NeverInline def cost: Int = rewritableMethod
  @NeverInline def dataSize: Long = rewritableMethod

  @NeverInline def get: Costed[T] = rewritableMethod
  @NeverInline def getOrElse(default: Costed[T]): Costed[T] = rewritableMethod
  @NeverInline def fold[B](ifEmpty: Costed[B], f: Costed[T => B]): Costed[B] = rewritableMethod
  @NeverInline def isEmpty: Costed[Boolean] = rewritableMethod
  @NeverInline def isDefined: Costed[Boolean] = rewritableMethod
  @NeverInline def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod
  @NeverInline def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod
  @NeverInline def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}

