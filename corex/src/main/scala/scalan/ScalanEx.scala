package scalan

import java.lang.reflect.Method

import scalan.compilation.GraphVizConfig
import scalan.meta.ScalanAst._
import scalan.primitives._
import scalan.staged.TransformingEx
import scalan.util.{ReflectionUtil, StringUtil}
import spire.syntax.all.cfor

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

trait BaseEx extends Base with DefRewriting { self: ScalanEx =>

  object IdSupply {
    private var _nextId = 0
    @inline final def nextId = { _nextId += 1; _nextId }
  }

  case class NumericRand[T](bound: Ref[T], id: Int = IdSupply.nextId)(implicit val eT: Elem[T]) extends BaseDef[T] {
    override def transform(t: Transformer) = NumericRand(t(bound))
  }

  def random[T](bound: Ref[T])(implicit n: Numeric[T]): Ref[T] =
    NumericRand(bound)(bound.elem)

  implicit class EntityElemExtensions[A <: Def[_]](e: Elem[A]) {
    def asEntityElem = e.asInstanceOf[EntityElem[A]]
  }

  implicit class ElemOpsForEntities[T](e: Elem[T]) {
    def isConcrete = isConcreteElem(e)
  }

  def isConcreteElem(e: TypeDesc): Boolean = e match {
    case _: BaseElem[_] =>
      true
    case e: EntityElem[_] if !isConcreteModuloTypeArgs(e) =>
      false
    case e: Elem[_] =>
      e.typeArgsDescs.forall(isConcreteElem)
    case _: Cont[_] => true
  }

  protected def isConcreteModuloTypeArgs(e: EntityElem[_]) = e match {
    case _: ViewElem[_, _] => true
    case _ => false
  }

  implicit class RepDefViewOps[T <: Def[_]](x: Ref[T]) {
    def convertTo[R <: Def[_]](implicit eR: Elem[R]): Ref[R] =
      eR match {
        case entE: EntityElem[R] @unchecked => entE.convert(x)
        case _ => !!!(s"Cannot convert $x to a value of type ${eR.name}: EntityElem expected but ${eR.getClass.getSimpleName} found", x)
      }
  }

  def getStagedFunc(name: String): Ref[_] = {
    val clazz = this.getClass
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Ref[_]]
  }

  def canBeInvoked(mc: MethodCall): Boolean = {
    val okFlags = !(mc.neverInvoke && !mc.isAdapterCall)
    val should = canBeInvoked(mc.receiver.node, mc.method, mc.args.toArray)
    okFlags && should
  }

  // FIXME this is a hack, this should be handled in Passes
  // The problem is that rewriting in ProgramGraph.transform is non-recursive
  // We need some way to make isInvokeEnabled local to graph
  type InvokeTester = (Def[_], Method) => Boolean

  // we need to always invoke these for creating default values
  case class NamedUnpackTester(name: String, tester: UnpackTester) extends UnpackTester {
    def apply(e: Elem[_]) = tester(e)
  }

  case class NamedInvokeTester(name: String, tester: InvokeTester) extends InvokeTester {
    def apply(d: Def[_], m: Method) = tester(d, m)
  }

  private val isCompanionApply: InvokeTester = NamedInvokeTester("isCompanionApply",
    (_, m) => m.getName == "apply" && m.getDeclaringClass.getName.endsWith("CompanionCtor")
  )

  protected def initialInvokeTesters: ArrayBuffer[InvokeTester] = {
    val res = new ArrayBuffer[InvokeTester](16)
    res += isCompanionApply
    res
  }
  private lazy val invokeTesters: ArrayBuffer[InvokeTester] = initialInvokeTesters

  def addInvokeTester(pred: InvokeTester): Unit = {
    invokeTesters += pred
  }

  def removeInvokeTester(pred: InvokeTester): Unit = {
    invokeTesters -= pred
  }

  def resetTesters() = {
    invokeTesters.clear()
    invokeTesters ++= initialInvokeTesters
    unpackTesters = initialUnpackTesters
  }

  protected def invokeAll = true

  override def isInvokeEnabled(d: Def[_], m: Method) = invokeAll || {
    if (_currentPass != null) {
      _currentPass.isInvokeEnabled(d, m).getOrElse {
        invokeTesters.exists(_(d, m))
      }
    }
    else
      invokeTesters.exists(_(d, m))
  }

  protected def hasFuncArg(args: Array[AnyRef]): Boolean = {
    cfor(0)(_ < args.length, _ + 1) { i =>
      val found = args(i) match {
        case f: Function0[_] => true
        case f: Function1[_, _] => true
        case f: Function2[_, _, _] => true
        case _ => false
      }
      if (found) return true
    }
    false
  }

  override protected def canBeInvoked(d: Def[_], m: Method, args: Array[AnyRef]) = {
    m.getDeclaringClass.isAssignableFrom(d.getClass) && {
      isInvokeEnabled(d, m) ||
          // If method arguments include Scala functions, the method can't be staged directly.
          // In most cases it just stages the functions and calls a method which _can_ be staged.
          hasFuncArg(args) || {
        // Methods can only be staged if they return Ref[_]. For such methods
        // the JVM return type is Object if the method is defined in abstract context
        // and Exp if defined in staged context.
        // If neither holds, the method again should be invoked immediately.
        val returnClass = m.getReturnType
        !(returnClass == classOf[AnyRef] || returnClass == classOf[Sym])
      }
    }
  }

  def decompose[T](d: Def[T]): Option[Ref[T]] = None

  val performViewsLifting: Boolean = true

  def rewriteViews[T](d: Def[T]): Ref[_] = null

  def unapplyViews[T](s: Ref[T]): Option[Unpacked[T]] = None
}

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
        sumElement(TypeDesc(a, env).asElem, TypeDesc(b, env).asElem)
      case STpeTuple(List(a, b)) =>
        pairElement(TypeDesc(a, env).asElem, TypeDesc(b, env).asElem)
      case STpeFunc(a, b) =>
        funcElement(TypeDesc(a, env).asElem, TypeDesc(b, env).asElem)
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
