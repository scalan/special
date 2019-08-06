package scalan

import java.lang.reflect.{InvocationTargetException, Method}
import java.util.Objects

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.{Success, Try}
import org.objenesis.ObjenesisStd
import net.sf.cglib.proxy.{InvocationHandler, Factory, Enhancer}
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.util.{ReflectionUtil, StringUtil, ScalaNameUtil}
import debox.{Buffer => DBuffer}
import spire.syntax.all.cfor

import scala.collection.mutable.ArrayBuffer

trait MethodCalls extends Base with GraphVizExport { self: Scalan =>

  def getStagedFunc(name: String): Ref[_] = {
    val clazz = this.getClass
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Ref[_]]
  }

  def delayInvoke = throw new DelayInvokeException

  // call mkMethodCall instead of constructor
  case class MethodCall private[MethodCalls](receiver: Sym, method: Method, args: Seq[AnyRef], neverInvoke: Boolean)
                                            (val resultType: Elem[Any], val isAdapterCall: Boolean = false) extends Def[Any] {

    override def mirror(t: Transformer): Ref[Any] = {
      val len = args.length
      val args1 = new Array[AnyRef](len)
      cfor(0)(_ < len, _ + 1) { i =>
        args1(i) = transformProductParam(args(i), t).asInstanceOf[AnyRef]
      }
      val receiver1 = t(receiver)
      // in the case neverInvoke is false, the method is invoked in rewriteDef
      mkMethodCall(receiver1, method, args1, neverInvoke, isAdapterCall, resultType).asInstanceOf[Ref[Any]]
    }

    override def toString = {
      val methodStr = method.toString.replace("java.lang.", "").
        replace("public ", "").replace("abstract ", "")
      s"MethodCall($receiver, $methodStr, [${args.mkString(", ")}], $neverInvoke)"
    }

    def tryInvoke: InvokeResult =
      if (neverInvoke && !isAdapterCall) {
        InvokeImpossible
      } else {
        invokeMethod[InvokeResult](
          receiver, method, args.toArray,
          { res => InvokeSuccess(res.asInstanceOf[Sym]) },
          { InvokeFailure(_) },
          { InvokeImpossible }
        )
      }

//    override protected def initContent(): Unit = {
//      _elements = Array()
//      val buf = DBuffer.ofSize[Sym](100)
//      buf += receiver
//      Def.addSyms(args, buf)
//      _syms = buf.toArray()
//    }
//
//    override def elements: Array[AnyRef] = !!!(s"MethodCall.elements is not defined and shouldn't be called")

    import scalan.util.CollectionUtil.TraversableOps
    override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || {
      other match {
        case other: MethodCall =>
          receiver == other.receiver &&
          method == other.method &&
          resultType.name == other.resultType.name &&
          neverInvoke == other.neverInvoke &&
          isAdapterCall == other.isAdapterCall &&
          args.length == other.args.length &&
          args.sameElements2(other.args) // this is required in case method have T* arguments
        case _ => false
      }
    }

    override lazy val hashCode: Int = {
      var h = receiver.hashCode() * 31 + method.hashCode()
      h = h * 31 + resultType.name.hashCode
      h = h * 31 + (if(neverInvoke) 1 else 0)
      h = h * 31 + (if(isAdapterCall) 1 else 0)
      h = h * 31 + args.hashCode()
      h
    }
  }

  case class NewObject[A](eA: Elem[A], args: Seq[Any], neverInvoke: Boolean) extends BaseDef[A]()(eA) {
    override def transform(t: Transformer) = NewObject(eA, t(args), neverInvoke)
  }

  def mkMethodCall(receiver: Sym, method: Method, args: Seq[AnyRef],
                   neverInvoke: Boolean, isAdapterCall: Boolean, resultElem: Elem[_]): Sym = {
    reifyObject(MethodCall(receiver, method, args, neverInvoke)(resultElem.asElem[Any], isAdapterCall))
  }

  @tailrec
  private def baseCause(e: Throwable): Throwable = e match {
    case e: java.lang.reflect.UndeclaredThrowableException => baseCause(e.getCause)
    case e: net.sf.cglib.proxy.UndeclaredThrowableException => baseCause(e.getCause)
    case e: InvocationTargetException => baseCause(e.getCause)
    case e: ExceptionInInitializerError => baseCause(e.getCause)
    case e => e
  }

  def canBeInvoked(mc: MethodCall) = {
    val okFlags = !(mc.neverInvoke && !mc.isAdapterCall)
    val should = shouldInvoke(mc.receiver.node, mc.method, mc.args.toArray)
    okFlags && should
  }

  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = d match {
    case mc: MethodCall if !canBeInvoked(mc) => "blue"
    case no: NewObject[_] if no.neverInvoke => "darkblue"
    case _ => super.nodeColor(td, d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case MethodCall(obj, method, args, _) =>
      val methodCallStr =
        s"${ScalaNameUtil.cleanScalaName(method.getName)}(${args.mkString(", ")})"
      if (obj.isCompanionType) {
        s"$obj.$methodCallStr"
      } else {
        val className = ScalaNameUtil.cleanNestedClassName(method.getDeclaringClass.getName)
        s"$obj.$className.$methodCallStr"
      }
    case NewObject(eA, args, _) =>
      val className = ScalaNameUtil.cleanNestedClassName(eA.sourceType.name)
      s"new $className(${args.mkString(", ")})"
    case _ => super.formatDef(d)
  }

  /** This method is called for each MethodCall node which is about to be added to the graph.
    * This means `mc` has been examined by all the rewrite rules, but has not need rewritten.
    * Now, if this method returns null, then mc will be added to the graph.
    * However, in this method, `mc` can be examined by a second set of RW rules
    * (kind of lower priority rules). These rules kind of context dependent, because at this
    * point we know that the first RW set didn't triggered any rewrite. */
  def rewriteNonInvokableMethodCall(mc: MethodCall): Ref[_] = null

  def newObjEx[A](args: Any*)(implicit eA: Elem[A]): Ref[A] = {
    reifyObject(NewObject[A](eA, args, true))
  }

  def unrefDelegate[T <: AnyRef](x: Ref[T])(implicit ct: ClassTag[T]): T = {
    val d = x.node
    if (d.isInstanceOf[Const[_]])
      d.asInstanceOf[Const[T]@unchecked].x
    else
      getDelegate(x, ct)
  }

  /** Used to cache generated delegate classes along with instantiated instances of the class.*/
  case class CachedDelegateClass(delegateClass: Class[_ <: AnyRef], instances: AVHashMap[Sym, AnyRef])

  private lazy val delegateCache = AVHashMap[ClassTag[_], CachedDelegateClass](100)
  private lazy val objenesis = new ObjenesisStd

  private def getDelegate[T](x: Ref[T], ct: ClassTag[T]): T = {
    val cachedOpt = delegateCache.get(ct)
    val entry = if (cachedOpt.isEmpty) {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[DelegatedInvocationHandler[_]])
      val delegateClass = e.createClass().asSubclass(classOf[AnyRef])
      val entry = CachedDelegateClass(delegateClass, AVHashMap[Sym, AnyRef](100))
      delegateCache.put(ct, entry)
      entry
    } else
      cachedOpt.get

    val delegateOpt = entry.instances.get(x)
    if (delegateOpt.isEmpty) {
      val delegateInstance = objenesis.newInstance(entry.delegateClass).asInstanceOf[Factory]
      delegateInstance.setCallback(0, new DelegatedInvocationHandler(x))
      entry.instances.put(x, delegateInstance)
      delegateInstance.asInstanceOf[T]
    }
    else
      delegateOpt.get.asInstanceOf[T]
  }

  private def invokeMethod[A](receiver: Sym, m: Method, args: Array[AnyRef],
                              onInvokeSuccess: AnyRef => A,
                              onInvokeException: Throwable => A,
                              onNoMethodFound: => A): A = {
    def tryInvoke(obj: Any, m: Method): A = {
      try {
        val res = m.invoke(obj, args: _*)
        onInvokeSuccess(res)
      } catch {
        case e: Exception => onInvokeException(baseCause(e))
      }
    }
    val d = receiver.node
    def findMethodLoop(m: Method): Option[Method] =
      if (shouldInvoke(d, m, args))
        Some(m)
      else
        None

    findMethodLoop(m) match {
      case Some(m1) =>
        tryInvoke(d, m1)
      case None =>
        onNoMethodFound
    }
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

  protected def invokeAll = true

  def isInvokeEnabled(d: Def[_], m: Method) = invokeAll || {
    if (_currentPass != null) {
      _currentPass.isInvokeEnabled(d, m).getOrElse {
        invokeTesters.exists(_(d, m))
      }
    }
    else
      invokeTesters.exists(_(d, m))
  }

  protected def shouldInvoke(d: Def[_], m: Method, args: Array[AnyRef]) = {
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

  protected def hasFuncArg(args: Array[AnyRef]): Boolean =
    args.exists {
      case f: Function0[_] => true
      case f: Function1[_, _] => true
      case f: Function2[_, _, _] => true
      case _ => false
    }

  // stack of receivers for which MethodCall nodes should be created by InvocationHandler
  protected var methodCallReceivers: List[Sym] = Nil

  private def invokeMethod(obj: AnyRef, methodName: String): AnyRef = {
    try {
      val method = obj.getClass.getMethod(methodName)
      try {
        val result = method.invoke(obj)
        result
      } catch {
        case e: Exception =>
          !!!(s"Failed to invoke $methodName of object $obj", e)
      }
    } catch {
      case _: NoSuchMethodException =>
        !!!(s"Failed to find method with name $methodName of object $obj")
    }
  }

  sealed trait InvokeResult

  case class InvokeSuccess(result: Ref[_]) extends InvokeResult
  case class InvokeFailure(exception: Throwable) extends InvokeResult
  case object InvokeImpossible extends InvokeResult

  class DelegatedInvocationHandler[T](receiver: Ref[T]) extends InvocationHandler {
    override def toString = s"ExpInvocationHandler(${receiver.toStringWithDefinition})"

    def invoke(delegate: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) Array.empty[AnyRef] else _args

      val res = invokeMethod(receiver, m, args, identity, {
        case cause =>
          throwInvocationException("Method invocation", cause, receiver, m, args)
      }, {
        !!!(s"Invocation handler is only supported for successful pass: ExpInvocationHandler($receiver).invoke($delegate, $m, ${args.toSeq})")
      })
      res
    }
  }

  def throwInvocationException(whatFailed: String, cause: Throwable, receiver: Sym, m: Method, args: Seq[Any]) = {
    val buf = DBuffer.empty[Sym]
    buf += receiver
    Def.extractSyms(args, buf)
    val deps = buf.toArray()
    !!!(s"$whatFailed (${receiver.varNameWithType}).${m.getName}(${args.mkString(", ")}) failed", baseCause(cause), deps: _*)
  }
}