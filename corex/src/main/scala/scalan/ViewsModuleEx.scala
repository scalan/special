package scalan

trait ViewsModuleEx extends ViewsModule with BaseEx { self: ScalanEx =>
  import IsoUR._

  object HasViews {
    def unapply[T](s: Rep[T]): Option[Unpacked[T]] =
      if (performViewsLifting)
        unapplyViews(s)
      else None
  }

  // for simplifying unapplyViews
  protected def trivialUnapply[T](s: Rep[T]) = (s, identityIso(s.elem))

  override def unapplyViews[T](s: Rep[T]): Option[Unpacked[T]] = (s match {
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
    def unapply[T](e: Rep[T]): Option[Unpacked[T]] =
      e match {
        case Def(UnpackableDef(source, iso: Iso[a, T] @unchecked)) =>
          Some((source.asInstanceOf[Rep[a]], iso))
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
    def unapply[A,C](l: Rep[A => C]): Option[UnpackedLambdaResult[A,C]] = l match {
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
    case Tup(HasViews(a, iso1: Iso[a, c]), b: Rep[b]) =>
      PairView((asRep[a](a), b), iso1, identityIso(b.elem)).self

    // Rule: (a, V(b, iso2)) ==> V((a,b), PairIso(id, iso2))
    case Tup(a: Rep[a], HasViews(b, iso2: Iso[b, d])) =>
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
    //    case ParallelExecute(nJobs:Rep[Int], f@Def(Lambda(_, _, _, HasViews(_, iso: Iso[a, b])))) =>
    //      implicit val ea = iso.eFrom
    //      val parRes = ParallelExecute(nJobs, fun { i => iso.from(f(i)) })(iso.eFrom)
    //      ViewArray(parRes, iso)

    // Rule: loop(V(start, iso), step, isMatch) ==> iso.to(loop(start, iso.to >> step >> iso.from, iso.to >> isMatch))
    //    case LoopUntil(HasViews(startWithoutViews, iso: Iso[a, b]), step, isMatch) =>
    //      val start1 = startWithoutViews.asRep[a]
    //      implicit val eA = iso.eFrom
    //      implicit val eB = iso.eTo
    //      val step1 = fun { (x: Rep[a]) =>
    //        val x_viewed = iso.to(x)
    //        val res_viewed = step.asRep[b => b](x_viewed) // mirrorApply(step.asRep[b => b], x_viewed)
    //      val res = iso.from(res_viewed)
    //        res
    //      }
    //      val isMatch1 = fun { (x: Rep[a]) =>
    //        val x_viewed = iso.to(x)
    //        val res = isMatch.asRep[b => Boolean](x_viewed) // mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
    //        res
    //      }
    //      val loopRes = LoopUntil(start1, step1, isMatch1)
    //      iso.to(loopRes)

    case _ => super.rewriteViews(d)
  }
}
