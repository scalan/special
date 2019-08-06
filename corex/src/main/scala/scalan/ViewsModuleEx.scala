package scalan

import scala.collection.mutable.{Map => MutMap}

trait ViewsModuleEx extends ViewsModule with BaseEx { self: ScalanEx =>
  import IsoUR._
  import AbsorbFirstUnitIso._
  import AbsorbSecondUnitIso._
  
  object HasViews {
    def unapply[T](s: Ref[T]): Option[Unpacked[T]] =
      if (performViewsLifting)
        unapplyViews(s)
      else None
  }

  object UnpackableElem {
    private val elems = MutMap.empty[(String, Elem[_]), Option[Iso[_, _]]]
    def unapply(e: Elem[_]) = elems.getOrElseUpdate((currentPass.name, e), {
      val iso = getIsoByElem(e)
      iso match {
        case Def(i: IsoUR[_,_]) =>
          if (i.isIdentity)
            None
          else
            Some(iso)
        case _ => None
      }
    })
  }

  trait IsoBuilder { def apply[T](e: Elem[T]): Iso[_,T] }

  object PairIsos {
    def fromElem[A,B](pe: PairElem[A,B]) = (getIsoByElem(pe.eFst), getIsoByElem(pe.eSnd))

    def unapply[T](e: Elem[T]): Option[(PairElem[_,_], Iso[_,_], Iso[_,_])] = e match {
      case pe: PairElem[a,b] if pe.eFst != UnitElement && pe.eSnd != UnitElement =>
        fromElem(pe) match {
          case (iso1: Iso[s, a] @unchecked, iso2: Iso[t, b] @unchecked) => Some((pe, iso1, iso2))
          case _ => None
        }
      case _ => None
    }
  }

  def getIsoByElem[T](e: Elem[T]): Iso[_, T] = {
    if (e.isInstanceOf[EntityElem[_]] && !shouldUnpack(e)) {
      identityIso(e).asInstanceOf[Iso[_, T]]
    } else if (currentPass.config.shouldUnpackTuples) {
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          val res: Iso[_, S] = e match {
            case PairIsos(_, iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              if (iso1.isIdentity && iso2.isIdentity) {
                // recursion base (structs)
                val sIso = structToPairIso[s,t,a,b](iso1, iso2)
                val flatIso = flatteningIso(sIso.eFrom)
                flatIso >> sIso.asIso[Struct,S]
              } else {
                val pIso = pairIso(iso1, iso2)
                val deepIso = getIsoByElem(pIso.eFrom)
                deepIso >> pIso.asIso[(s,t),S]
              }
            case _ =>
              getIsoByElem(e)
          }
          res
        }
      })
    } else {
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          val res: Iso[_, S] = e match {
            case PairIsos(_, iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              if (iso1.isIdentity && iso2.isIdentity) {
                // recursion base
                pairIso(iso1, iso2).asInstanceOf[Iso[_, S]]
              } else {
                getIsoByElem(e)
              }
            case _ =>
              getIsoByElem(e)
          }
          res
        }
      })
    }
  }

  // TODO this caching doesn't work with structs (uncomment and check test("flatteningIso"))
  // private[this] val isoCache = MutMap.empty[Elem[_], Iso[_, _]]
  def buildIso[T](e: Elem[T], builder: IsoBuilder): Iso[_, T] = //isoCache.getOrElseUpdate(
    (//classOf[Iso[_, _]], Seq(e)),
    e match {
      case ie: IsoURElem[_, _, _] =>
        identityIso(ie)
      case ve: ViewElem[_, _] =>
        val eFrom = ve.iso.eFrom
        val deepIso = builder(eFrom)
        if (deepIso.isIdentity)
          ve.iso
        else
          deepIso >> ve.iso
      case pe: PairElem[a,b] => (pe.eFst, pe.eSnd) match {
        case (`UnitElement`, eB) =>
          builder(eB) match { case isoB: Iso[s,b] @unchecked =>
            RAbsorbFirstUnitIso(isoB)
          }
        case (eA, `UnitElement`) =>
          builder(eA) match { case isoA: Iso[s,a] @unchecked =>
            RAbsorbSecondUnitIso(isoA)
          }
        case (eA, eB) =>
          (builder(eA), builder(eB)) match {
            case (iso1: Iso[s,a] @unchecked, iso2: Iso[t,b] @unchecked) =>
              val pIso = pairIso(iso1,iso2)
              val deepIso = builder(pIso.eFrom)
              deepIso >> pIso
          }
      }
      case pe: SumElem[a,b] =>
        val iso1 = builder(pe.eLeft)
        val iso2 = builder(pe.eRight)
        sumIso(iso1,iso2)
      case fe: FuncElem[a,b] =>
        val iso1 = builder(fe.eDom)
        val iso2 = builder(fe.eRange)
        funcIso(iso1,iso2)
      case ae: ThunkElem[_] =>
        val iso = builder(ae.eItem)
        thunkIso(iso)

      //    case ee1: EntityElem1[_,_,_] =>
      //      val iso = getIsoByElem(ee1.eItem)
      //      TODO implement using ContIso
      case ee: EntityElem[_] =>
        identityIso(ee)
      case be: BaseElem[_] =>
        identityIso(be)
      case se: StructElem[_] =>
        identityIso(se)
      case _ => !!!(s"Don't know how to build iso for element $e")
    }
    ).asInstanceOf[Iso[_,T]]

  // for simplifying unapplyViews
  protected def trivialUnapply[T](s: Ref[T]) = (s, identityIso(s.elem))

  override def unapplyViews[T](s: Ref[T]): Option[Unpacked[T]] = (s match {
    case Def(d: SLeft[l, r]) =>
      val left = d.left
      val eRight = d.eRight
      (unapplyViews(left), UnpackableElem.unapply(eRight)) match {
        case (None, None) => None
        case (opt1, opt2) =>
          val (sv1, iso1) = opt1.getOrElse(trivialUnapply(left))
          val iso2 = opt2.getOrElse(identityIso(eRight)).asInstanceOf[Iso[_, r]]
          Some((sv1.asLeft(iso2.eFrom), sumIso(iso1, iso2)))
      }
    case Def(d: SRight[l, r]) =>
      val eLeft = d.eLeft
      val right = d.right
      (UnpackableElem.unapply(eLeft), unapplyViews(right)) match {
        case (None, None) => None
        case (opt1, opt2) =>
          val (sv2, iso2) = opt2.getOrElse(trivialUnapply(right))
          val iso1 = opt1.getOrElse(identityIso(eLeft)).asInstanceOf[Iso[_, l]]
          Some((sv2.asRight(iso1.eFrom), sumIso(iso1, iso2)))
      }
    case _ =>
      UnpackableExp.unapply(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[Unpacked[T]] =
      d match {
        case view: View[a, T]@unchecked => Some((view.source, view.iso))
        // TODO make UserTypeDef extend View with lazy iso0/source?
        case _ =>
          None
      }
  }

  object UnpackableExp {
    def unapply[T](e: Ref[T]): Option[Unpacked[T]] =
      e match {
        case Def(UnpackableDef(source, iso: Iso[a, T] @unchecked)) =>
          Some((source.asInstanceOf[Ref[a]], iso))
        case _ =>
          val eT = e.elem
          eT match {
            case UnpackableElem(iso: Iso[a, T] @unchecked) =>
              Some((iso.from(e), iso))
            case _ => None
          }
      }
  }

  object LambdaResultHasViews {
    def unapply[A,C](l: Ref[A => C]): Option[UnpackedLambdaResult[A,C]] = l match {
      case Def(Lambda(_, _, _, HasViews(_, iso: Iso[b, C]@unchecked))) =>
        Some((l, iso))
      case _ => None
    }
  }

  override def rewriteViews[T](d: Def[T]) = d match {
    // Rule: (V(a, iso1), V(b, iso2)) ==> V((a,b), PairIso(iso1, iso2))
    case Tup(HasViews(a, iso1: Iso[a, c]), HasViews(b, iso2: Iso[b, d])) =>
      PairView((asRep[a](a), asRep[b](b)), iso1, iso2)

    // Rule: (V(a, iso1), b) ==> V((a,b), PairIso(iso1, id))
    case Tup(HasViews(a, iso1: Iso[a, c]), b: Ref[b]) =>
      PairView((asRep[a](a), b), iso1, identityIso(b.elem)).self

    // Rule: (a, V(b, iso2)) ==> V((a,b), PairIso(id, iso2))
    case Tup(a: Ref[a], HasViews(b, iso2: Iso[b, d])) =>
      PairView((a, asRep[b](b)), identityIso(a.elem), iso2).self

    // Rule: PairView(source, iso1, _)._1  ==> iso1.to(source._1)
    case First(Def(view@PairView(source,_,_))) =>
      view.iso1.to(source._1)

    // Rule: PairView(source, _, iso2)._2  ==> iso2.to(source._2)
    case Second(Def(view@PairView(source,_,_))) =>
      view.iso2.to(source._2)

    // Rule: PairView(PairView(source, i2), i1)  ==> PairView(source, PairIso(composeIso(i1.iso1, i2.iso1), composeIso(i1.iso2, i2.iso2)))
    case v1@PairView(Def(v2@PairView(source,_,_)),_,_) => {
      val pIso1 = composeIso(v1.iso1, v2.iso1)
      val pIso2 = composeIso(v1.iso2, v2.iso2)
      PairView(source, pIso1, pIso2)
    }

    // Rule: UnpackView(V(source, iso))  ==> source
    case UnpackView(Def(UnpackableDef(source, _)), _) => source

    // Rule: ParExec(nJobs, f @ i => ... V(_, iso)) ==> V(ParExec(nJobs, f >> iso.from), arrayiso(iso))
    //    case ParallelExecute(nJobs:Ref[Int], f@Def(Lambda(_, _, _, HasViews(_, iso: Iso[a, b])))) =>
    //      implicit val ea = iso.eFrom
    //      val parRes = ParallelExecute(nJobs, fun { i => iso.from(f(i)) })(iso.eFrom)
    //      ViewArray(parRes, iso)

    // Rule: loop(V(start, iso), step, isMatch) ==> iso.to(loop(start, iso.to >> step >> iso.from, iso.to >> isMatch))
    //    case LoopUntil(HasViews(startWithoutViews, iso: Iso[a, b]), step, isMatch) =>
    //      val start1 = startWithoutViews.asRep[a]
    //      implicit val eA = iso.eFrom
    //      implicit val eB = iso.eTo
    //      val step1 = fun { (x: Ref[a]) =>
    //        val x_viewed = iso.to(x)
    //        val res_viewed = step.asRep[b => b](x_viewed) // mirrorApply(step.asRep[b => b], x_viewed)
    //      val res = iso.from(res_viewed)
    //        res
    //      }
    //      val isMatch1 = fun { (x: Ref[a]) =>
    //        val x_viewed = iso.to(x)
    //        val res = isMatch.asRep[b => Boolean](x_viewed) // mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
    //        res
    //      }
    //      val loopRes = LoopUntil(start1, step1, isMatch1)
    //      iso.to(loopRes)

    case _ => super.rewriteViews(d)
  }
}
