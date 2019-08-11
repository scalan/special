package scalan

import scalan.compilation.GraphVizConfig
import scalan.meta.ScalanAst._
import scalan.primitives._
import scalan.staged.TransformingEx
import scalan.util.StringUtil

class ScalanEx extends Scalan
  with Blocks
  with ConvertersModule
  with StringOps
  with Metadata
  with RewriteRules
  with BaseEx
  with ModulesEx
  with TransformingEx
  with ViewsModuleEx
  with ThunksEx
  with StructsEx
  with ConvertersModuleEx
{ self =>

  type TypeArgSubst = Map[String, TypeDesc]
  type TypePredicate = Elem[_] => Boolean
  def AllTypes(e: Elem[_]): Boolean = true
  val emptySubst = Map.empty[String, TypeDesc]

  implicit class TypeDescOps(d: TypeDesc) {
    def asElemOption[B]: Option[Elem[B]] = if (isElem) Some(d.asInstanceOf[Elem[B]]) else None
    def asCont[C[_]]: Cont[C] = d.asInstanceOf[Cont[C]] // TODO remove
    def asContOption[C[_]]: Option[Cont[C]] = if (isCont) Some(d.asInstanceOf[Cont[C]]) else None
    def isElem: Boolean = d.isInstanceOf[Elem[_]]
    def isCont: Boolean = d.isInstanceOf[Cont[Any] @unchecked]
  }

  def pairifyElems(es: Iterator[Elem[_]]): Elem[_] = {
    def step(a: Elem[_], b: Elem[_], tail: Iterator[Elem[_]]): Elem[_] = {
      if (tail.hasNext) {
        val c = tail.next()
        pairElement(a, step(b, c, tail))
      }
      else {
        pairElement(a, b)
      }
    }
    val a = es.next()
    val b = es.next()
    step(a, b, es)
  }

  override def resetContext() = {
    super.resetContext()
    metadataPool = Map.empty[Sym, MetaNode]
  }

  /** Used by Graphviz dot file generator to specify color of the node. */
  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = d match {
    case mc: MethodCall if !canBeInvoked(mc) => "blue"
    case no: NewObject[_] => "darkblue"
    case _ => super.nodeColor(td, d)
  }

  override protected def formatMetadata(s: Sym): List[String] = {
    val metadata = s.allMetadata.meta
    if (metadata.nonEmpty)
      "Metadata:" :: metadata.map { case (k, v) => s"$k:${formatConst(v.value)}" }.toList
    else
      Nil
  }

  protected def rewriteUntilFixPoint[T](start: Ref[T], mn: MetaNode, rw: Rewriter): Ref[T] = {
    var res = start
    var curr: Ref[T] = res
    do {
      curr = res
      setAllMetadata(curr, mn)
      res = rw(curr)
    } while (res != curr)
    res
  }

  override protected[scalan] def toExp[T](d: Def[T], newSym: => Ref[T]): Ref[T] = {
    var res = findOrCreateDefinition(d, newSym)
    var currSym = res
    var currDef = d
    do {
      currSym = res
      val ns = rewrite(currSym).asInstanceOf[Ref[T]]
      ns match {
        case null =>
          currDef = null
        case Def(someOtherD) =>
          res = ns
          currDef = someOtherD
        case _ =>
          res = ns
          currDef = null
      }
    } while (res != currSym && currDef != null)
    res
  }

  def toTpeExpr(d: TypeDesc): STpeExpr = d match {
    case e: Elem[_] => e.toTpeExpr
    case _ => ???
  }

  implicit class ElemOps(e: Elem[_]) {
    def toTpeExpr: STpeExpr = e match {
      case _ if e == UnitElement => TpeUnit
      case _ if e == BooleanElement => TpeBoolean
      case _ if e == ByteElement => TpeByte
      case _ if e == ShortElement => TpeShort
      case _ if e == IntElement => TpeInt
      case _ if e == LongElement => TpeLong
      case _ if e == FloatElement => TpeFloat
      case _ if e == DoubleElement => TpeDouble
      case _ if e == StringElement => TpeString
      case _ if e == CharElement => TpeChar
      case pe: PairElem[_,_] =>
        val a = pe.eFst.toTpeExpr
        val b = pe.eSnd.toTpeExpr
        STpeTuple(List(a,b))
      case pe: FuncElem[_,_] =>
        val a = pe.eDom.toTpeExpr
        val b = pe.eRange.toTpeExpr
        STpeFunc(a,b)
      case ee: EntityElem[a] =>
        STraitCall(ee.entityName, ee.typeArgs.map { case (_, (a, _)) => self.toTpeExpr(a) }.toList)
      //      case ae: StructElem[a] =>
      //        val tpes = ae.fields.map { case (name, el) =>
      //          BaseType(name, List(el))
      //        }
      //        StructType(tpes.toList)


      //      case be: BaseTypeElem1[a,tExt,cBase] =>
      //        val a = Type(be.eItem)
      //        BaseType(be.runtimeClass.getSimpleName, List(a))
      //      case be: BaseTypeElem[tBase,tExt] =>
      //        BaseType(be.runtimeClass.getSimpleName, Nil)


      case _ => sys.error(s"Cannot perform toTpeExpr for $e")
    }
  }

  private def callMethod(obj: AnyRef, methodName: String, descClasses: Array[Class[_]], paramDescs: List[AnyRef]): TypeDesc = {
    try {
      val method = obj.getClass.getMethod(methodName, descClasses: _*)
      try {
        val result = method.invoke(obj, paramDescs: _*)
        result.asInstanceOf[Elem[_]]
      } catch {
        case e: Exception =>
          !!!(s"Failed to invoke $methodName with parameters $paramDescs", e)
      }
    } catch {
      case e: Exception =>
        !!!(s"Failed to find elem-creating method with name $methodName with parameters $paramDescs: ${e.getMessage}")
    }
  }

  object TypeDesc {
    def apply(tpe: STpeExpr, env: TypeArgSubst): TypeDesc = tpe match {
      case STpePrimitive(name,_) =>
        val methodName = name + "Element"
        callMethod(self, methodName, Array(), Nil)
      case STraitCall("$bar", List(a, b)) =>
        sumElement(asElem(TypeDesc(a, env)), asElem(TypeDesc(b, env)))
      case STpeTuple(List(a, b)) =>
        pairElement(asElem(TypeDesc(a, env)), asElem(TypeDesc(b, env)))
      case STpeFunc(a, b) =>
        funcElement(asElem(TypeDesc(a, env)), asElem(TypeDesc(b, env)))
      case STraitCall(name, Nil) =>
        env.get(name) match {
          case Some(t) => t
          case None =>
            val methodName = StringUtil.lowerCaseFirst(name + "Element")
            val obj = getEntityObject(name).getOrElse(self)
            callMethod(obj, methodName, Array(), List())
        }
      case STraitCall(name, args) =>
        val argDescs = args.map(p => TypeDesc(p, env))
        val argClasses = argDescs.map {
          case e: Elem[_] => classOf[Elem[_]]
          case c: Cont[_] => classOf[Cont[Any]]
          case d => !!!(s"Unknown type descriptior $d")
        }.toArray[Class[_]]
        val methodName = StringUtil.lowerCaseFirst(name + "Element")
        val obj = getEntityObject(name).getOrElse(self)
        callMethod(obj, methodName, argClasses, argDescs)

      case _ => !!!(s"Unexpected STpeExpr: $tpe")
    }
  }

  implicit class STpeExprOpsForTypeDesc(tpe: STpeExpr) {
    def toTypeDesc(env: TypeArgSubst = Map()): TypeDesc = TypeDesc(tpe, env)
  }

  class ArgElem(val tyArg: STpeArg) extends Elem[Any] with Serializable with scala.Equals {
    def argName = tyArg.name
    override def getName(f: TypeDesc => String) = {
      if (typeArgs.isEmpty)
        tyArg.name
      else {
        val typeArgString = typeArgsDescs.map(f).mkString(", ")
        s"${tyArg.name}[$typeArgString]"
      }
    }

    def variance = tyArg.variance
    def isCovariant = tyArg.isCovariant
    def tyExpr = tyArg.toTraitCall
    def toDesc(env: Map[ArgElem,TypeDesc]): TypeDesc = env.get(this) match {
      case Some(d) => d
      case None =>
        !!!(s"Don't know how to convert ArgElem $this to TypeDesc")
    }

    override def <:<(e: Elem[_]) = if (this == e) true else false

    override def canEqual(other: Any) = other.isInstanceOf[ArgElem]
    override def equals(other: Any) = other match {
      case other: ArgElem =>
        this.eq(other) ||
            (other.canEqual(this) && this.tyArg == other.tyArg)
      case _ => false
    }
    override lazy val hashCode = tyArg.hashCode()
  }
  object ArgElem {
    def apply(name: String): ArgElem = new ArgElem(STpeArg(name, None, Nil))
    def apply(a: STpeArg) = new ArgElem(a)
    def unapply(t: ArgElem): Option[STpeArg] = Some(t.tyArg)
  }

}