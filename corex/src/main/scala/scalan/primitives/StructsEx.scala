package scalan.primitives

import scalan.util.CollectionUtil
import scalan.{ScalanEx, Nullable}

trait StructsEx extends Structs with StructItemsModule with StructKeysModule { self: ScalanEx =>
  import IsoUR._

  case class ViewStruct[A, B](source: Ref[A])(val iso: Iso[A, B])
      extends View[A, B] {
    override def transform(t: Transformer) = ViewStruct(t(source))(t(iso))
    override def toString = s"ViewStruct[${iso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewStruct[_, _] => source == v.source && iso.eTo == v.iso.eTo
      case _ => false
    }
  }

  override def unapplyViews[T](s: Ref[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewStruct[a, b]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object StructsRewriter extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] = asRep[T](x match {
      case Def(FieldGet(v)) => v
      case _ => x
    })
  }

  object FieldGet {
    def unapply[T](d: FieldApply[T]): Option[Ref[T]] = d match {
      case FieldApply(Def(SimpleStruct(_, fs)), fn) =>
        val optItem = fs.find { case (n, _) => n == fn }
        optItem.map(x => asRep[T](x._2))
      case _ => None
    }
  }

  object SameStructAs {
    def unapply[A](d: Def[A]): Nullable[Ref[A]] = d match {
      case Struct(tag, fields) =>
        fields.headOption match {
          case Some((_, Def(Field(possibleSourceStruct, _)))) if d.resultType == possibleSourceStruct.elem =>
            val eachFieldComesFromPossibleSourceStruct = fields.forall {
              case (name, Def(Field(`possibleSourceStruct`, name1))) if name == name1 =>
                true
              case _ =>
                false
            }
            if (eachFieldComesFromPossibleSourceStruct)
              Nullable(asRep[A](possibleSourceStruct))
            else
              Nullable.None
          case _ => Nullable.None
        }
      case _ => Nullable.None
    }
  }

  def shouldUnpackTuples = currentPass.config.shouldUnpackTuples
  def shouldExtractFields = currentPass.config.shouldExtractFields
  def shouldSlice = currentPass.config.shouldSlice

  override def rewriteDef[T](d: Def[T]): Sym = d match {
    case FieldGet(v) if shouldExtractFields => v
    case SameStructAs(s) => s
    case _ => super.rewriteDef(d)
  }

  case class StructToPairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])
      extends IsoUR[Struct, (B1, B2)] {
    override def transform(t: Transformer) = StructToPairIso(t(iso1), t(iso2))
    override def equals(other: Any) = other match {
      case iso: StructsEx#StructToPairIso[_, _, _, _] =>
        (this eq iso) || (iso1 == iso.iso1 && iso2 == iso.iso2)
      case _ => false
    }

    implicit def eA1 = iso1.eFrom
    implicit def eA2 = iso2.eFrom
    implicit def eB1 = iso1.eTo
    implicit def eB2 = iso2.eTo
    lazy val eFrom = tuple2StructElement(iso1.eFrom, iso2.eFrom)
    lazy val eTo = element[(B1, B2)]
    lazy val resultType = asElem[IsoUR[Struct, (B1, B2)]](new ConcreteIsoElem[Struct, (B1, B2), StructToPairIso[A1, A2, B1, B2]](eFrom, eTo))


    override def from(p: Ref[(B1, B2)]) =
      struct(tupleFN(0) -> iso1.from(p._1), tupleFN(1) -> iso2.from(p._2))

    override def to(struct: Ref[Struct]) = {
      Pair(iso1.to(struct.getUnchecked[A1](tupleFN(0))), iso2.to(struct.getUnchecked[A2](tupleFN(1))))
    }
  }

  def structToPairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[Struct, (B1, B2)] =
    reifyObject(StructToPairIso[A1, A2, B1, B2](iso1, iso2))

  def structToPairIso[A:Elem,B:Elem]: Iso[Struct, (A, B)] = structToPairIso[A,B,A,B](identityIso[A], identityIso[B])
  def structToPairIso[A,B](pe: Elem[(A,B)]): Iso[Struct, (A, B)] = structToPairIso[A,B](pe.eFst, pe.eSnd)

  case class StructIso[S <: Struct, T <: Struct](eFrom: StructElem[S], eTo: StructElem[T], itemIsos: Seq[Iso[_,_]])
      extends IsoUR[S, T] {
    assert(eFrom.isEqualType(itemIsos.map(_.eFrom)))
    assert(eTo.isEqualType(itemIsos.map(_.eTo)))
    override def transform(t: Transformer) = StructIso[S, T](eFrom, eTo, itemIsos.map(t(_)))
    override def equals(other: Any) = other match {
      case iso: StructsEx#StructIso[_, _] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }

    override def from(y: Ref[T]) = {
      val items = eFrom.fields.zip(eTo.fields).zip(itemIsos).map {
        case (((fnS, feS), (fnT, feT)), iso: Iso[s,t] @unchecked) =>
          fnS -> iso.from(y.getUnchecked[t](fnT))
        case (_, nonIso) => !!!(s"Iso expected but found $nonIso", self, y, nonIso)
      }
      asRep[S](struct(items))
    }
    override def to(x: Ref[S]) = {
      val items = eFrom.fields.zip(eTo.fields).zip(itemIsos).map {
        case (((fnS, feS), (fnT, feT)), iso: Iso[s,t] @unchecked) =>
          fnT -> iso.to(x.getUnchecked[s](fnS))
        case (_, nonIso) => !!!(s"Iso expected but found $nonIso", self, x, nonIso)
      }
      asRep[T](struct(items))
    }

    lazy val resultType = asElem[IsoUR[S, T]](new ConcreteIsoElem[S, T, StructIso[S, T]](eFrom, eTo))
  }

  def structIso[S <: Struct, T <: Struct](eFrom: StructElem[S], eTo: StructElem[T], itemIsos: Seq[Iso[_,_]]): Iso[S, T] =
    reifyObject(StructIso(eFrom, eTo, itemIsos))

  case class Link(field: String, nestedField: String, nestedElem: Elem[_], flatName: String)

  case class FlatteningIso[T <: Struct](eTo: StructElem[T], flatIsos: Map[String, Iso[_,_]], links: Seq[Link])
      extends IsoUR[Struct,T] {
    override def transform(t: Transformer) = FlatteningIso(eTo, flatIsos.mapValues(t(_)), links)
    override def equals(other: Any) = other match {
      case iso: StructsEx#FlatteningIso[_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }

    val eFrom = tupleStructElement(links.map(_.nestedElem): _*)
    lazy val resultType = asElem[IsoUR[Struct, T]](new ConcreteIsoElem[Struct, T, FlatteningIso[T]](eFrom, eTo))

    val groups = links.groupBy(_.field)

    def to(x: Ref[Struct]) = {
      val items = eTo.fields.map { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso: Iso[a, _] @unchecked) =>
            val projectedStruct = struct(g.map {
              link => link.nestedField -> x.getUntyped(link.flatName)
            }: _*)
            val s = iso.to(asRep[a](projectedStruct))
            (fn, s)
          case _ =>
            assert(g.length == 1, s"Many fields $g can't relate to the single field $fn without iso")
            (fn, x.getUntyped(g(0).flatName))
        }
      }
      struct(eTo.structTag, items: _*)
    }

    def from(y: Ref[T]) = {
      val items = eTo.fields.flatMap { case (fn, fe) =>
        val g = groups(fn)
        flatIsos.get(fn) match {
          case Some(iso: Iso[_, a] @unchecked) =>
            val nestedStruct = asRep[Struct](iso.from(y.getUnchecked[a](fn)))
            // nestedStruct is guaranteed to be a Ref[Struct], because iso can be either IdentityIso on a struct or FlatteningIso
            g.map { link =>
              link.flatName -> nestedStruct.getUntyped(link.nestedField)
            }
          case _ =>
            List(g(0).flatName -> y.getUntyped(fn))
        }
      }
      struct(items: _*)
    }
  }

  /**
    * Flattens all subtrees of structs in [[e]].
    * Types other than structs are considered either as internal nodes or as leaves.
    * @param e descriptor of struct type
    * @return an isomorphism in which [[e]] is given by param and `eFrom` is flattened [[e]] preserving
    *         related order of the components
    */
  def getFlatteningIso[T](e: Elem[T]): Iso[_,T] = (e match {
    // a == T, but Scala can't infer the type bound if T is used below
    case se: StructElem[Struct] @unchecked =>
      val flatIso = flatteningIso(se).asInstanceOf[Iso[_, T]]
      flatIso match {
        case Def(_: IdentityIso[T] @unchecked) =>
          flatIso
        case Def(_: FlatteningIso[T] @unchecked) =>
          flatIso.eFrom match {
            // TODO Actually, we currently know s == Struct. Is extra complexity needed?
            case eFrom: StructElem[Struct]@unchecked =>
              val isos = eFrom.fields.map { case (fn,fe) => (fn, buildIso(fe, flatteningBuilder)) }
              val eFromNew = structElement(isos.map { case (fn, iso) => fn -> iso.eFrom })
              val sIso = reifyObject(new StructIso(eFromNew, eFrom, isos.map(_._2)))
              sIso >> flatIso.asInstanceOf[Iso[Struct,T]]
          }
      }
    case _ =>
      buildIso(e, flatteningBuilder)
  }).asInstanceOf[Iso[_,T]]

  val flatteningBuilder = new IsoBuilder { def apply[S](e: Elem[S]) = getFlatteningIso(e) }

  def flatteningIso[T <: Struct](eTo: StructElem[T]): Iso[_, T] = {
    val flatIsos: Map[String, Iso[_, _]] = eTo.fields.collect {
      case (fn, fe: StructElem[Struct]@unchecked) => (fn, flatteningIso(fe))
    }.toMap

    if (flatIsos.isEmpty)
      return identityIso(eTo)

    // relate resulting field types by original field name
    val fromFields = eTo.fields.flatMap {
      case (fn, fe) =>
        flatIsos.get(fn) match {
          case Some(iso) =>
            iso.eFrom match {
              case flatElem: StructElem[_] =>
                flatElem.fields.map { case (nestedName, nestedE) => (fn, nestedName -> nestedE) }
              case _ => !!!(s"StructElem is expected as eFrom of flattened Iso $iso")
            }
          case None => List((fn, "" -> fe))
        }
    }

    val links =
      fromFields.zipWithIndex.map {
        case ((fn, (nestedN, nestedE)), i) => Link(fn, nestedN, nestedE, tupleFN(i))
      }

    val res: Iso[_, T] = reifyObject(FlatteningIso(eTo, flatIsos, links))
    res
  }

  def getStructToPairsIso[T](implicit e: Elem[T]): Iso[_,T] = (e match {
    case pe: PairElem[a,b] =>
      val iso1 = getStructToPairsIso(pe.eFst)
      val iso2 = getStructToPairsIso(pe.eSnd)
      val res = structToPairIso(iso1, iso2)
      res
    case _ =>
      buildIso(e, new IsoBuilder {
        def apply[S](e: Elem[S]) = {
          getStructToPairsIso(e)
        }
      })
  }).asInstanceOf[Iso[_,T]]

  def getStructWrapperIso[T](implicit e: Elem[T]): Iso[_,T] = {
    getStructToPairsIso(e) match {
      case iso: Iso[s,T] @unchecked =>
        val flatIso = getFlatteningIso[s](iso.eFrom)
        flatIso >> iso
    }
  }

  case class MergeIso[T <: Struct](eTo: StructElem[T]) extends IsoUR[Struct,T] {
    override def transform(t: Transformer) = MergeIso(eTo)
    override def equals(other: Any) = other match {
      case iso: MergeIso[_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }

    val eFrom = structElement(eTo.fields.flatMap {
      case (fn, fe: StructElem[_]) => fe.fields
      case (_, nonStructElem) => !!!(s"StructElem expected but found $nonStructElem", self)
    })

    lazy val resultType = asElem[IsoUR[Struct, T]](new ConcreteIsoElem[Struct, T, MergeIso[T]](eFrom, eTo))

    def to(x: Ref[Struct]) = {
      val items = eTo.fields.map {
        case (outerN, outerE: StructElem[_]) =>
          val s = struct(outerE.fields.map { case (innerN, innerE) => innerN -> x.getUntyped(innerN) })
          outerN -> s
        case (_, nonStructElem) => !!!(s"StructElem expected but found $nonStructElem", self, x)
      }
      struct(eTo.structTag, items: _*)
    }

    def from(y: Ref[T]) = {
      val items = eTo.fields.flatMap {
        case (outerN, outerE: StructElem[_]) =>
          val s = asRep[Struct](y.getUntyped(outerN))
          outerE.fields.map { case (innerN, innerE) => innerN -> s.getUntyped(innerN) }
        case (_, nonStructElem) => !!!(s"StructElem expected but found $nonStructElem", self, y)
      }
      struct(items: _*)
    }
  }

  def getStructMergeIso[T](implicit e: Elem[T]): Iso[_,T] = (e match {
    case se: StructElem[_] =>
      reifyObject(MergeIso(asElem[Struct](se)))
    case _ =>
      !!!(s"Don't know how merge non struct $e")
  }).asInstanceOf[Iso[_,T]]

  def pairifyStruct[A <: Struct](se: Elem[A]): Elem[_] = {
    CollectionUtil.foldRight[(String, Elem[_]), Elem[_]](se.fields)(_._2) { case ((fn,fe), e) => pairElement(fe, e) }
  }

  def unzipMany[T](tuple: Ref[_], list: List[T]): List[Ref[_]] = {
    val pair = asRep[(Any, Any)](tuple)
    list match {
      case Nil => List(tuple)
      case x :: Nil => List(tuple)
      case x :: y :: Nil => List(pair._1, pair._2)
      case x :: xs =>
        pair._1 :: unzipMany(pair._2, xs)
    }
  }

  case class PairifyIso[A, AS <: Struct](eTo: Elem[AS]) extends IsoUR[A, AS] {
    val eFrom: Elem[A] = asElem[A](pairifyStruct(eTo))
    override def transform(t: Transformer) = PairifyIso(eTo)

    def from(y: Ref[AS]) =  {
      val res = CollectionUtil.foldRight[String, Ref[_]](eTo.fieldNames)(y.getUntyped(_)) {
        case (fn, s) => Pair(y.getUntyped(fn), s)
      }
      asRep[A](res)
    }

    override def to(x: Ref[A]) = {
      val items = unzipMany(x, eTo.fields.toList)
      val fields = eTo.fieldNames.zip(items.map(asRep[Any](_)))
      asRep[AS](struct(fields))
    }

    override def equals(other: Any) = other match {
      case iso: PairifyIso[_,_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }
    lazy val resultType = asElem[IsoUR[A, AS]](new ConcreteIsoElem[A, AS, PairifyIso[A, AS]](eFrom, eTo))
  }

  def structWrapper[A,B](f: Ref[A => B]): Ref[Any => Any] = {
    val wrapperFun = (getStructWrapperIso[A](f.elem.eDom),
        getStructWrapperIso[B](f.elem.eRange)) match {
      case (inIso: Iso[a, A] @unchecked, outIso: Iso[b, B] @unchecked) =>
        outIso.fromFun << f << inIso.toFun
    }
    asRep[Any => Any](wrapperFun)
  }
  def structWrapperIn[A,B](f: Ref[A => B]): Ref[Any => B] = {
    val inIso = getStructWrapperIso[A](f.elem.eDom)
    val wrapperFun = inIso.toFun >> f
    asRep[Any => B](wrapperFun)
  }
  def structWrapperOut[A,B](f: Ref[A => B]): Ref[A => Any] = {
    val outIso = getStructWrapperIso[B](f.elem.eRange)
    val wrapperFun = f >> outIso.fromFun
    asRep[A => Any](wrapperFun)
  }

}
