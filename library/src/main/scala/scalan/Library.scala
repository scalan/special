package scalan

import special.collection._
import special.wrappers.{WrappersSpecModule, WrappersModule}
import scalan.util.{MemoizedFunc}

trait Library extends Scalan
  with WrappersModule
  with WrappersSpecModule
  with CollsModule
  with SizesModule
  with CostsModule
  with ConcreteSizesModule
  with ConcreteCostsModule
  with MonoidsModule
  with CostedOptionsModule {
  import WOption._
  import WRType._
  import Coll._; import CollBuilder._;
  import Size._
  import Costed._; import CostedBuilder._
  import CostedFunc._;
  import WSpecialPredef._

  type RSize[Val] = Rep[Size[Val]]
  type RCosted[A] = Rep[Costed[A]]
  type LazyRep[T] = MutableLazy[Rep[T]]

  private val _liftElemMemo = new MemoizedFunc({
    case eT: Elem[t] =>
      val lT = Liftables.asLiftable[Any, t](eT.liftable)
      liftableRType(lT).lift(eT.sourceType.asInstanceOf[RType[Any]])
  })
  implicit def liftElem[T](eT: Elem[T]): Rep[WRType[T]] = {
    _liftElemMemo(eT).asInstanceOf[Rep[WRType[T]]]  // asRep cannot be used for AnyRef
  }

  private val _specialPredef: LazyRep[WSpecialPredefCompanionCtor] = MutableLazy(RWSpecialPredef)
  def specialPredef: Rep[WSpecialPredefCompanionCtor] = _specialPredef.value

  override protected def onReset(): Unit = {
    _specialPredef.reset()
    _liftElemMemo.reset()
    super.onReset()
  }

  def zeroSize[V](eVal: Elem[V]): RSize[V] = asRep[Size[V]](eVal match {
    case pe: PairElem[a,b] => costedBuilder.mkSizePair(zeroSize[a](pe.eFst), zeroSize[b](pe.eSnd))
    case ce: CollElem[_,_] =>
      implicit val eItem = ce.eItem
      costedBuilder.mkSizeColl(colBuilder.fromItems(zeroSize(eItem)))
    case oe: WOptionElem[_,_] => costedBuilder.mkSizeOption(specialPredef.some(zeroSize(oe.eItem)))
    case _: BaseElem[_] | _: EntityElem[_] => costedBuilder.mkSizePrim(0L, eVal)
    case _ => !!!(s"Cannot create zeroSize($eVal)")
  })

  private val CM = CollMethods
  private val CBM = CollBuilderMethods
  private val WOptionM = WOptionMethods
  private val SPCM = WSpecialPredefCompanionMethods

  def colBuilder: Rep[CollBuilder]
  def costedBuilder: Rep[CostedBuilder]
  def intPlusMonoid: Rep[Monoid[Int]]
  def longPlusMonoid: Rep[Monoid[Long]]

  val intPlusMonoidValue = new special.collection.MonoidBuilderInst().intPlusMonoid
  val longPlusMonoidValue = new special.collection.MonoidBuilderInst().longPlusMonoid

  override def rewriteDef[T](d: Def[T]) = d match {
    case CM.length(ys) => ys.rhs match {
      // Rule: xs.map(f).length  ==> xs.length
      case CM.map(xs, _) =>
        xs.length
      // Rule: replicate(len, v).length => len
      case CBM.replicate(_, len, _) =>
        len
      // Rule: Const[Coll[T]](coll).length =>
      case CollConst(coll, _) =>
        coll.length
      // Rule: Coll(items @ Seq(x1, x2, x3)).length => items.length
      case CBM.fromItems(_, items, _) =>
        items.length
      case _ => super.rewriteDef(d)
    }

    // Rule: replicate(l, x).zip(replicate(l, y)) ==> replicate(l, (x,y))
    case CM.zip(CBM.replicate(b1, l1, v1), CBM.replicate(b2, l2, v2)) if b1 == b2 && l1 == l2 =>
      b1.replicate(l1, Pair(v1, v2))

    case CM.map(xs, _f) => _f.rhs match {
      case IdentityLambda() => xs
      case _ => xs.rhs match {
        // Rule: replicate(l, v).map(f) ==> replicate(l, f(v))
        case CBM.replicate(b, l, v: Rep[a]) =>
          val f = asRep[a => Any](_f)
          b.replicate(l, Apply(f, v, false))
        case _ => super.rewriteDef(d)
      }
    }

    case CM.sum(xs, m) => m.rhs match {
      case _: IntPlusMonoid => xs.rhs match {
        case CollConst(coll, lA) if lA.eW == IntElement =>
          coll.asInstanceOf[SColl[Int]].sum(intPlusMonoidValue)
        case CBM.replicate(_, n, x: Rep[Int] @unchecked) =>
          x * n
        case _ => super.rewriteDef(d)
      }
      case _: LongPlusMonoid => xs.rhs match {
        case CollConst(coll, lA) if lA.eW == LongElement =>
          coll.asInstanceOf[SColl[Long]].sum(longPlusMonoidValue)
        case CBM.replicate(_, n, x: Rep[Long] @unchecked) =>
          x * n.toLong
        case _ => super.rewriteDef(d)
      }
      case _ => super.rewriteDef(d)
    }

    // Rule: opt.fold(None, x => Some(x)) ==> opt
    case WOptionM.fold(opt, Def(ThunkDef(SPCM.none(_), _)), Def(Lambda(_, _, x, SPCM.some(y)))) if x == y => opt

    case WOptionM.getOrElse(opt, _) => opt.rhs match {
      // Rule: Some(x).getOrElse(_) ==> x
      case SPCM.some(x) => x
      case WOptionConst(Some(x), lA) => lA.lift(x)
      case _ => super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }

  override def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): AnyRef = e match {
    case _: CollElem[_,_] => mc match {
      case CollMethods.map(xs, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.selfType, mc.isAdapterCall)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _ =>
      super.invokeUnlifted(e, mc, dataEnv)
  }

//  implicit class CostedFuncOps[A,B](fC: Rep[Costed[A => B]]) {
//    def applyCosted(x: Rep[Costed[A]]): Rep[Costed[B]] = {
//      val fC_elem = fC.elem.asInstanceOf[CostedElem[A => B,_]].eVal
//      implicit val eA = fC_elem.eDom
//      implicit val eB = fC_elem.eRange
//      val res = tryConvert(
//            element[CostedFunc[Unit,A,B]], element[Costed[B]], fC,
//            fun { f: Rep[CostedFunc[Unit,A,B]] => f.func(x) })
//      res
//    }
//  }
}
