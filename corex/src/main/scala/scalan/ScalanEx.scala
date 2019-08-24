package scalan

import java.lang.reflect.Method

import net.sf.cglib.proxy.{Factory, Enhancer, InvocationHandler}
import org.objenesis.ObjenesisStd
import scalan.compilation.GraphVizConfig
import scalan.meta.ScalanAst._
import scalan.primitives._
import scalan.staged.TransformingEx
import scalan.util.StringUtil

import scala.reflect.ClassTag

class ScalanEx extends Scalan
  with Blocks
  with ViewsModule
  with ConvertersModule
  with StringOps
  with Metadata
  with RewriteRules
  with BaseEx
  with TuplesEx
  with TypeSum
  with Structs
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

  override protected def partsIterator(td: TypeDesc) = td match {
    case se: StructElem[_] =>
      se.fieldElems.iterator
    case _ => super.partsIterator(td)
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

  /** Create delegate instance suitable for method invocation.
    * It is used when T is a class or a trait and the node referred by x doesn't conform to T.
    * This method returns dynamically constructed instance, which conforms to T.
    * Whenever a method of T is called on that instance, the call is intercepted and
    * `DelegatedInterceptionHandler.invoke` method is called, then a new MethodCall can
    * be constructed (which is befavior by default).
    */
  override protected def unrefDelegate[T <: AnyRef](x: Ref[T])(implicit ct: ClassTag[T]): T = {
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

  /** Construct delegate instance for the given type and receiver object. */
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

  /** Handles intercepted invocations of method on delegates. */
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

  /** A more convenient then `fun` to call with explicit eA. */
  final def typedfun[A, B](eA: Elem[A])(f: Ref[A] => Ref[B]): Ref[A => B] =
    fun(f)(Lazy(eA))

  def composeBi[A, B, C, D](f: Ref[A => B], g: Ref[A => C])(h: (Ref[B], Ref[C]) => Ref[D]): Ref[A => D] = {
    typedfun(f.elem.eDom) { x => h(f(x), g(x)) }
  }

  implicit class BooleanFuncOps[A](f: Ref[A => Boolean]) {
    def &&&(g: Ref[A => Boolean]) =
      if (f == g)
        f
      else
        composeBi(f, g)(_ && _)

    def |||(g: Ref[A => Boolean]) =
      if (f == g)
        f
      else
        composeBi(f, g)(_ || _)

    def !!! = typedfun(f.elem.eDom) { x => !f(x) }
  }

}
