package scalan

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Method

import spire.syntax.all.cfor

trait BaseEx extends TypeDescs with DefRewriting { self: ScalanEx =>

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
