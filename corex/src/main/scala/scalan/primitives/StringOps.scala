package scalan.primitives

import scalan.{ScalanEx, BaseEx}

trait StringOps extends BaseEx { self: ScalanEx =>
  implicit class StringOpsCls(lhs: Ref[String]) {
    def toInt = StringToInt(lhs)
    def toDouble = StringToDouble(lhs)
    def length = StringLength(lhs)
    def apply(index: Ref[Int]) = string_apply(lhs, index)
    def substring(start: Ref[Int], end: Ref[Int]) = string_substring(lhs, start, end)
    def +(rhs: Ref[String]) = StringConcat(lhs, rhs)
    def startsWith(rhs: Ref[String]) = StringStartsWith(lhs, rhs)
    def endsWith(rhs: Ref[String]) = StringEndsWith(lhs, rhs)
    def contains(rhs: Ref[String]) = StringContains(lhs, rhs)
    def matches(rhs: Ref[String]) = StringMatches(lhs, rhs)
  }

  object StringObject {
    lazy val empty = toRep("")
  }

  val StringToInt = new UnOp[String, Int]("toInt", _.toInt)
  val StringToDouble = new UnOp[String, Double]("toDouble", _.toDouble)
  val StringLength = new UnOp[String, Int]("length", _.length)

  val StringConcat = new EndoBinOp[String]("+", _ + _)
  val StringContains = new BinOp[String, Boolean]("contains", _.contains(_))
  val StringStartsWith = new BinOp[String, Boolean]("startsWith", _.startsWith(_))
  val StringEndsWith = new BinOp[String, Boolean]("endsWith", _.endsWith(_))
  val StringMatches = new BinOp[String, Boolean]("matches", _.matches(_))

  case class StringSubstring(str: Ref[String], start: Ref[Int], end: Ref[Int]) extends BaseDef[String] {
    override def transform(t: Transformer) = StringSubstring(t(str), t(start), t(end))
  }
  case class StringCharAt(str: Ref[String], index: Ref[Int]) extends BaseDef[Char] {
    override def transform(t: Transformer) = StringCharAt(t(str), t(index))
  }

  def string_substring(str: Ref[String], start: Ref[Int], end: Ref[Int]): Ref[String] = StringSubstring(str, start, end)
  def string_apply(str: Ref[String], index: Ref[Int]): Ref[Char] = StringCharAt(str, index)

  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyBinOp(op, x, Def(Const(""))) if op == StringConcat =>
      x
    case ApplyBinOp(op, Def(Const("")), x) if op == StringConcat =>
      x
    case ApplyBinOp(op, x, Def(Const(""))) if op == StringStartsWith || op == StringEndsWith =>
      toRep(true)
    case _ => super.rewriteDef(d)
  }
}
