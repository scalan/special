package special.collection

import special.SpecialPredef

import scalan.meta.RType
import RType._
import special.SpecialPredef._

import scalan.NeverInline

/** NOTE: Option is a special case of Either, such that Option[T] is isomorphic to Either[Unit, T].
  * Keeping this in mind, we however define constructions for Option separately. */
trait CostedOption[T] extends ConcreteCosted[Option[T]]
{
  //  @NeverInline
  //  def cost: Int = left.cost max right.cost + builder.ConstructSumCost
  //  @NeverInline
  //  def dataSize: Long = left.dataSize max right.dataSize + builder.SumTagSize
  def get: Costed[T]
  def getOrElse(default: Costed[() => T]): Costed[T]
  def fold[B](ifEmpty: Costed[() => B], f: Costed[T => B]): Costed[B]
  def isEmpty: Costed[Boolean]
  def isDefined: Costed[Boolean]
  def filter(p: Costed[T => Boolean]): Costed[Option[T]]
  def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]]
  def map[B](f: Costed[T => B]): Costed[Option[B]]
}

class CostedSome[T](val costedValue: Costed[T]) extends CostedOption[T] {
  def value: Option[T] = SpecialPredef.some(costedValue.value)
  def dataSize: Long = builder.SumTagSize + costedValue.dataSize
  def cost: Int = costedValue.cost + builder.ConstructSumCost  // see doc comments for CostedOption
  def get: Costed[T] = costedValue
  def getOrElse(default: Costed[() => T]): Costed[T] = costedValue

  @NeverInline
  def fold[B](ifEmpty: Costed[() => B], f: Costed[T => B]): Costed[B] = rewritableMethod

  def isEmpty: Costed[Boolean] = new CostedPrim(false, costedValue.cost + builder.SelectFieldCost, 1L)

  def isDefined: Costed[Boolean] = new CostedPrim(true, costedValue.cost + builder.SelectFieldCost, 1L)

  @NeverInline
  def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod

  @NeverInline
  def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod

  @NeverInline
  def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}

class CostedNone[T](val cost: Int)(implicit val eT: RType[T]) extends CostedOption[T] {
  def value: Option[T] = SpecialPredef.none[T](eT)
  def dataSize: Long = builder.SumTagSize
  def get: Costed[T] = builder.costedValue(builder.defaultValue(eT), SpecialPredef.some(cost))

  @NeverInline
  def getOrElse(default: Costed[() => T]): Costed[T] = rewritableMethod

  @NeverInline
  def fold[B](ifEmpty: Costed[() => B], f: Costed[T => B]): Costed[B] = rewritableMethod

  def isEmpty: Costed[Boolean] = new CostedPrim(true, cost + builder.SelectFieldCost, 1L)

  def isDefined: Costed[Boolean] = new CostedPrim(false, cost + builder.SelectFieldCost, 1L)

  @NeverInline
  def filter(p: Costed[T => Boolean]): Costed[Option[T]] = rewritableMethod

  @NeverInline
  def flatMap[B](f: Costed[T => Option[B]]): Costed[Option[B]] = rewritableMethod

  @NeverInline
  def map[B](f: Costed[T => B]): Costed[Option[B]] = rewritableMethod
}


