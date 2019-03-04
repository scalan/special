package scalan

import java.lang.reflect.Method
import java.util.Objects

import special.collection._
import special.wrappers.{WrappersSpecModule, WrappersModule}
import scalan.util.ReflectionUtil

trait Library extends Scalan
  with WrappersModule
  with WrappersSpecModule
  with CollsModule
  with CollsOverArraysModule
  with CostsModule
  with ConcreteCostsModule
  with MonoidsModule
  with MonoidInstancesModule
  with CostedOptionsModule {
  import WArray._; import WOption._
  import WRType._
  import Coll._; import CollBuilder._;
  import CReplColl._
  import Costed._
  import CostedFunc._;
  import WSpecialPredef._

  trait Sized[Val] { node: Costed[Val] =>
    lazy val dataSize: Rep[Long] = {
      sizeOf(value)
    }
  }

  protected def checkSize[Val](value: Rep[Val], size: Rep[Long]) = {
    val info = extractSizeData(size)
    assert(correctSizeDataType(value.elem, info.elem), msgIncorrectSizeDataStructure(value.elem, info.elem))
  }

  implicit def liftElem[T](eT: Elem[T]): Rep[WRType[T]] = {
    val lT = eT.liftable.asInstanceOf[Liftables.Liftable[Any, T]]
    liftableRType(lT).lift(eT.asInstanceOf[RType[Any]])
  }

  override def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = eA match {
    case ea: WArrayElem[_,_] => Objects.deepEquals(x, y)
    case _ => super.equalValues[A](x, y)
  }

  override protected def getResultElem(receiver: Sym, m: Method, args: List[AnyRef]): Elem[_] = receiver.elem match {
    case ae: WOptionElem[a, _] => m.getName match {
      case "getOrElse" =>
        val f = args(0).asInstanceOf[Rep[Thunk[Any]]]
        f.elem.eItem
      case _ => super.getResultElem(receiver, m, args)
    }
    case ae: WArrayElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        wArrayElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case ce: CollElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        collElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case b: CollBuilderElem[_] => m.getName match {
      case "apply" =>
        ReflectionUtil.overloadId(m) match {
          case Some("apply_items") =>
            val eItem = args(0).asInstanceOf[Seq[Sym]](0).elem
            collElement(eItem)
          case _ => super.getResultElem(receiver, m, args)
        }
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }

  private val WA = WArrayMethods
  private val CM = CollMethods
  private val CBM = CollBuilderMethods
  private val WOptionM = WOptionMethods
  private val SPCM = WSpecialPredefCompanionMethods

  object IsProjectFirst {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(First(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  object IsProjectSecond {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(Second(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  object IsNumericToInt {
    def unapply(d: Def[_]): Nullable[Rep[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToInt[_], x) => Nullable(x.asInstanceOf[Rep[A] forSome {type A}])
      case _ => Nullable.None
    }
  }
  object IsNumericToLong {
    def unapply(d: Def[_]): Nullable[Rep[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToLong[_], x) => Nullable(x.asInstanceOf[Rep[A] forSome {type A}])
      case _ => Nullable.None
    }
  }

  def colBuilder: Rep[CollBuilder]
  def costedBuilder: Rep[CostedBuilder]
  def intPlusMonoid: Rep[Monoid[Int]]
  def longPlusMonoid: Rep[Monoid[Long]]

  val intPlusMonoidValue = new special.collection.MonoidBuilderInst().intPlusMonoid
  val longPlusMonoidValue = new special.collection.MonoidBuilderInst().longPlusMonoid

  override def rewriteDef[T](d: Def[T]) = d match {
    case WA.length(WA.map(xs, _)) => xs.length
    case CM.length(CM.map(xs, _)) => xs.length

    case CM.length(CBM.replicate(_, len, _)) => len
    case CM.length(Def(CollConst(coll, _))) => coll.length
    case CM.length(CBM.fromArray(_, arr)) => arr.length
    case CM.length(CBM.fromItems(_, items, _)) => items.length
    case IsNumericToLong(Def(IsNumericToInt(x))) if x.elem == LongElement => x

    // Rule: replicate(l, x).zip(replicate(l, y)) ==> replicate(l, (x,y))
    case CM.zip(CBM.replicate(b1, l1, v1), CBM.replicate(b2, l2, v2)) if b1 == b2 && l1 == l2 =>
      b1.replicate(l1, Pair(v1, v2))

    // Rule: replicate(l, v).map(f) ==> replicate(l, f(v))
    case CM.map(CBM.replicate(b, l, v: Rep[a]), _f) =>
      val f = asRep[a => Any](_f)
      b.replicate(l, f(v))

    // Rule: xs.map(_._1).zip(xs.map(_._2)) ==> xs
    case WA.zip(WA.map(xs, IsProjectFirst(_)), WA.map(ys, IsProjectSecond(_))) if xs == ys => xs
    case CM.zip(CM.map(xs, IsProjectFirst(_)), CM.map(ys, IsProjectSecond(_))) if xs == ys => xs

    case WA.map(WA.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>
      implicit val ea = f.elem.eDom
      val xs = _xs.asRep[WArray[a]]
      val g  = _g.asRep[b => c]
      xs.map(fun { x: Rep[a] => g(f(x)) })
    case CM.map(CM.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>
      implicit val ea = f.elem.eDom
      val xs = _xs.asRep[Coll[a]]
      val g  = _g.asRep[b => c]
      xs.map(fun { x: Rep[a] => g(f(x)) })

    case CM.map(xs, Def(IdentityLambda())) => xs
    case CM.map(xs, Def(ConstantLambda(res))) =>
      RCReplColl(res, xs.length)

    case CM.sum(Def(CollConst(coll, lA)), Def(_: IntPlusMonoid)) if lA.eW == IntElement =>
      coll.asInstanceOf[SColl[Int]].sum(intPlusMonoidValue)
    case CM.sum(Def(CollConst(coll, lA)), Def(_: LongPlusMonoid)) if lA.eW == LongElement =>
      coll.asInstanceOf[SColl[Long]].sum(longPlusMonoidValue)

    case CM.sum(CBM.replicate(_, n, x: Rep[Int] @unchecked), Def(_: IntPlusMonoid)) =>
      x * n
    case CM.sum(CBM.replicate(_, n, x: Rep[Long] @unchecked), Def(_: LongPlusMonoid)) =>
      x * n.toLong
      
    // Rule: opt.fold(None, x => Some(x)) ==> opt
    case WOptionM.fold(opt, Def(ThunkDef(SPCM.none(_), _)), Def(Lambda(_, _, x, SPCM.some(y)))) if x == y => opt

    // Rule: Some(x).getOrElse(_) ==> x
    case WOptionM.getOrElse(SPCM.some(x), _) => x
    case WOptionM.getOrElse(Def(WOptionConst(Some(x), lA)), _) => lA.lift(x)

    case _ => super.rewriteDef(d)
  }

  override def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): AnyRef = e match {
    case _: CollBuilderElem[_] => mc match {
      case CollBuilderMethods.fromArray(b, xs) =>
        val newMC = mc.copy(args = mc.args :+ xs.elem.eItem)(mc.selfType, mc.isAdapterCall)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _: CollElem[_,_] => mc match {
      case CollMethods.map(xs, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.selfType, mc.isAdapterCall)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _: WArrayElem[_,_] => mc match {
      case WArrayMethods.map(xs, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.selfType, mc.isAdapterCall)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _ =>
      super.invokeUnlifted(e, mc, dataEnv)
  }

  protected override def correctSizeDataType[TVal, TSize](eVal: Elem[TVal], eSize: Elem[TSize]): Boolean = eVal match {
    case e: CollElem[_,_] => eSize.isInstanceOf[CollElem[_,_]]
    case e: WOptionElem[_,_] => eSize.isInstanceOf[WOptionElem[_,_]]
    case _ => super.correctSizeDataType(eVal, eSize)
  }

  override def calcSizeFromData[V,S](data: SizeData[V, S]): Rep[Long] = data.eVal match {
    case ce: CollElem[a,_] => asRep[Coll[Long]](data.sizeInfo).sum(longPlusMonoid)
    case oe: WOptionElem[a,_] => asRep[WOption[Long]](data.sizeInfo).getOrElse(Thunk(0L))
    case _ => super.calcSizeFromData(data)
  }

  implicit class CostedFuncOps[A,B](fC: Rep[Costed[A => B]]) {
    def applyCosted(x: Rep[Costed[A]]): Rep[Costed[B]] = {
      val fC_elem = fC.elem.asInstanceOf[CostedElem[A => B,_]].eVal
      implicit val eA = fC_elem.eDom
      implicit val eB = fC_elem.eRange
      val res = tryConvert(
            element[CostedFunc[Unit,A,B]], element[Costed[B]], fC,
            fun { f: Rep[CostedFunc[Unit,A,B]] => f.func(x) })
      res
    }
  }
}
