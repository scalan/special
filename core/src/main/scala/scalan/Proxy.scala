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
import scala.collection.mutable.ArrayBuffer

trait Proxy extends Base with GraphVizExport { self: Scalan =>
  import IsoUR._
  
  def getStagedFunc(name: String): Rep[_] = {
    val clazz = this.getClass
    val f = clazz.getDeclaredMethod(name)
    f.invoke(this).asInstanceOf[Rep[_]]
  }

  def delayInvoke = throw new DelayInvokeException

  // call mkMethodCall instead of constructor
  case class MethodCall private[Proxy](receiver: Sym, method: Method, args: List[AnyRef], neverInvoke: Boolean)
                                      (val selfType: Elem[Any], val isAdapterCall: Boolean = false) extends Def[Any] {

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


    //TODO optimize
    import scalan.util.CollectionUtil.TraversableOps
    override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || {
      other match {
        case other: MethodCall =>
          receiver == other.receiver &&
          method == other.method &&
          selfType.name == other.selfType.name &&
          neverInvoke == other.neverInvoke &&
          isAdapterCall == other.isAdapterCall &&
          args.length == other.args.length &&
          args.sameElements2(other.args) // this is required in case method have T* arguments
        case _ => false
      }
    }

    override lazy val hashCode: Int = {
      var h = receiver.hashCode() * 31 + method.hashCode()
      h = h * 31 + selfType.name.hashCode
      h = h * 31 + (if(neverInvoke) 1 else 0)
      h = h * 31 + (if(isAdapterCall) 1 else 0)
      h = h * 31 + args.hashCode()
      h
    }
  }

  case class NewObject[A](eA: Elem[A], args: List[Any], neverInvoke: Boolean) extends BaseDef[A]()(eA) {
    override def transform(t: Transformer) = NewObject(eA, t(args).toList, neverInvoke)
  }

  override def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
    // not the same as super because mkMethodCall can produce a more precise return type
    case mc @ MethodCall(receiver, method, args, neverInvoke) =>
      val args1 = args.map(transformProductParam(_, t).asInstanceOf[AnyRef])
      val receiver1 = t(receiver)
      // in the case neverInvoke is false, the method is invoked in rewriteDef
      mkMethodCall(receiver1, method, args1, neverInvoke, mc.isAdapterCall, mc.selfType).asInstanceOf[Rep[A]]
    case _ => super.transformDef(d, t)
  }

  def mkMethodCall(receiver: Sym, method: Method, args: List[AnyRef],
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
    val should = shouldInvoke(mc.receiver.rhs, mc.method, mc.args.toArray)
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
      if (obj.isCompanion) {
        s"$obj.$methodCallStr"
      } else {
        val className = ScalaNameUtil.cleanNestedClassName(method.getDeclaringClass.getName)
        s"$obj.$className.$methodCallStr"
      }
    case NewObject(eA, args, _) =>
      val className = ScalaNameUtil.cleanNestedClassName(eA.runtimeClass.getName)
      s"new $className(${args.mkString(", ")})"
    case _ => super.formatDef(d)
  }

  override def rewriteDef[T](d: Def[T]): Sym = d match {
    case call @ MethodCall(receiver, m, args, neverInvoke) =>
      call.tryInvoke match {
        // Rule: receiver.m(args) ==> body(m).subst{xs -> args}
        case InvokeSuccess(res) => res
        case InvokeFailure(e) if !e.isInstanceOf[DelayInvokeException] =>
          throwInvocationException("Method invocation in rewriteDef", e, receiver, m, args)
        case InvokeImpossible =>
          val res = rewriteNonInvokableMethodCall(call)
          if (res != null) res
          else
            super.rewriteDef(d)
      }
    case _ =>
      super.rewriteDef(d)
  }

  /** This method is called for each MethodCall node which is about to be added to the graph.
    * This means `mc` has been examined by all the rewrite rules, but has not need rewritten.
    * Now, if this method returns null, then mc will be added to the graph.
    * However, in this method, `mc` can be examined by a second set of RW rules
    * (kind of lower priority rules). These rules kind of context dependent, because at this
    * point we know that the first RW set didn't triggered any rewrite. */
  def rewriteNonInvokableMethodCall(mc: MethodCall): Rep[_] = null

  def newObjEx[A](args: Any*)(implicit eA: Elem[A]): Rep[A] = {
    reifyObject(NewObject[A](eA, args.toList, true))
  }

  private lazy val proxies = scala.collection.mutable.Map.empty[(Rep[_], ClassTag[_]), AnyRef]
  private lazy val objenesis = new ObjenesisStd

  def proxyOps[Ops <: AnyRef](x: Rep[Ops])(implicit ct: ClassTag[Ops]): Ops =
    x match {
      case Def(Const(c)) => c
      case _ => getProxy(x, ct)
    }

  // TODO optimize: call e.createClass only once per clazz
  private def getProxy[Ops](x: Rep[Ops], ct: ClassTag[Ops]) = {
    val proxy = proxies.getOrElseUpdate((x, ct), {
      val clazz = ct.runtimeClass
      val e = new Enhancer
      e.setClassLoader(clazz.getClassLoader)
      e.setSuperclass(clazz)
      e.setCallbackType(classOf[ExpInvocationHandler[_]])
      val proxyClass = e.createClass().asSubclass(classOf[AnyRef])
      val proxyInstance = objenesis.newInstance(proxyClass).asInstanceOf[Factory]
      proxyInstance.setCallback(0, new ExpInvocationHandler(x))
      proxyInstance
    })
    proxy.asInstanceOf[Ops]
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
    val d = receiver.rhs
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
//    res += isFieldGetter
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
        // Methods can only be staged if they return Rep[_]. For such methods
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

//  import Symbols._

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

  case class InvokeSuccess(result: Rep[_]) extends InvokeResult
  case class InvokeFailure(exception: Throwable) extends InvokeResult
  case object InvokeImpossible extends InvokeResult

  class ExpInvocationHandler[T](receiver: Rep[T]) extends InvocationHandler {
    override def toString = s"ExpInvocationHandler(${receiver.toStringWithDefinition})"

    def invoke(proxy: AnyRef, m: Method, _args: Array[AnyRef]) = {
      val args = if (_args == null) Array.empty[AnyRef] else _args

      val res = invokeMethod(receiver, m, args, identity, {
        case cause =>
          throwInvocationException("Method invocation", cause, receiver, m, args)
      }, {
        !!!(s"Invocation handler is only supported for successful pass: ExpInvocationHandler($receiver).invoke($proxy, $m, ${args.toSeq})")
      })
      res
    }
  }

  def throwInvocationException(whatFailed: String, cause: Throwable, receiver: Sym, m: Method, args: Seq[Any]) = {
    val buf = DBuffer.empty[Sym]
    buf += receiver
    Def.addSyms(args, buf)
    val deps = buf.toArray()
    !!!(s"$whatFailed (${receiver.toStringWithType}).${m.getName}(${args.mkString(", ")}) failed", baseCause(cause), deps: _*)
  }
}
