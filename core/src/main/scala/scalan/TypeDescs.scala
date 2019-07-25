package scalan

import java.lang.reflect.{InvocationTargetException, Method}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.{AnyValManifest, ClassTag}
import scalan.meta.ScalanAst._
import scalan.util._
import scalan.RType._
import scalan.util.ReflectionUtil.ClassOps
import spire.syntax.all._

import scala.collection.mutable

trait TypeDescs extends Base { self: Scalan =>

  type TypeArgSubst = Map[String, TypeDesc]
  type TypePredicate = Elem[_] => Boolean
  def AllTypes(e: Elem[_]): Boolean = true
  val emptySubst = Map.empty[String, TypeDesc]

  @inline def asElem[T](d: TypeDesc): Elem[T] = d.asInstanceOf[Elem[T]]

  implicit class TypeDescOps(d: TypeDesc) {
    def asElem[B]: Elem[B] = self.asElem[B](d)  // TODO remove
    def asElemOption[B]: Option[Elem[B]] = if (isElem) Some(d.asInstanceOf[Elem[B]]) else None
    def asCont[C[_]]: Cont[C] = d.asInstanceOf[Cont[C]] // TODO remove
    def asContOption[C[_]]: Option[Cont[C]] = if (isCont) Some(d.asInstanceOf[Cont[C]]) else None
    def isElem: Boolean = d.isInstanceOf[Elem[_]]
    def isCont: Boolean = d.isInstanceOf[Cont[Any] @unchecked]
  }

  type LElem[A] = Lazy[Elem[A]] // lazy element

  val SymClass = classOf[Sym]
  val CtClass = classOf[ClassTag[_]]

  type DataEnv = Map[Sym, AnyRef]

  /** State monad for symbols computed in an environment. */
  case class EnvRep[A](run: DataEnv => (DataEnv, Rep[A])) {
    def flatMap[B](f: Rep[A] => EnvRep[B]): EnvRep[B] = EnvRep { env =>
      val (env1, x) = run(env)
      val res = f(x).run(env1)
      res
    }
    def map[B](f: Rep[A] => Rep[B]): EnvRep[B] = EnvRep { env =>
      val (env1, x) = run(env)
      val y = f(x)
      (env1, y)
    }
  }
  object EnvRep {
    def add[T](entry: (Rep[T], AnyRef)): EnvRep[T] =
      EnvRep { env => val (sym, value) = entry; (env + (sym -> value), sym) }

    def lifted[ST, T](x: ST)(implicit lT: Liftables.Liftable[ST, T]): EnvRep[T] = EnvRep { env =>
      val xSym = lT.lift(x)
      val resEnv = env + (xSym -> x.asInstanceOf[AnyRef])
      (resEnv, xSym)
    }
  }

  sealed trait MethodDesc {
    def method: Method
  }
  case class RMethodDesc(method: Method) extends MethodDesc
  case class WMethodDesc(wrapSpec: WrapSpec, method: Method) extends MethodDesc

  // TODO optimize performance hot spot (45% of invokeUnlifted time)
  def getSourceValues(dataEnv: DataEnv, forWrapper: Boolean, stagedValues: AnyRef*): Seq[AnyRef] = {
    import OverloadHack._
    val limit = stagedValues.length
    val res = mutable.ArrayBuilder.make[AnyRef]()
    res.sizeHint(limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      val v = stagedValues.apply(i)
      v match {
        case s: Sym =>
          res += dataEnv(s)
        case vec: Seq[AnyRef]@unchecked =>
          res += getSourceValues(dataEnv, forWrapper, vec:_*)
//        case se: StructElem[_] if se.fields.length == 2 =>
//          val tA = se.fields(0)._2
//          val tB = se.fields(1)._2
//          PairType(tA, tB)
        case e: Elem[_] =>
          val arg =
            if (forWrapper) e.sourceType.classTag  // WrapSpec classes use ClassTag implicit arguments
            else e.sourceType
          res += arg
        case _: Overloaded => // filter out special arguments
      }
    }
    res.result()
  }

  trait TypeDesc extends Serializable {
    def getName(f: TypeDesc => String): String
    lazy val name: String = getName(_.name)

    // <> to delimit because: [] is used inside name; {} looks bad with structs.
    override def toString = s"${getClass.safeSimpleName}<$name>"
  }

  /**
    * Reified type representation in Scalan.
    *
    * @tparam A The represented type
    */
  @implicitNotFound(msg = "No Elem available for ${A}.")
  abstract class Elem[A] extends TypeDesc { _: scala.Equals =>
    import Liftables._

    def tag: WeakTypeTag[A]

    def buildTypeArgs: ListMap[String, (TypeDesc, Variance)] = EmptyTypeArgs
    lazy val typeArgs: ListMap[String, (TypeDesc, Variance)] = buildTypeArgs
    lazy val typeArgsDescs: Seq[TypeDesc] = {
      val b = mutable.ArrayBuilder.make[TypeDesc]()
      for (v <- typeArgs.valuesIterator) {
        b += v._1
      }
      b.result()
    }

    override def getName(f: TypeDesc => String) = {
      val className = this match {
        case be: BaseElemLiftable[_] =>
          be.sourceType.name
        case e =>
          val cl = e.getClass
          val name = cl.safeSimpleName
          name
      }
      if (typeArgs.isEmpty)
        className
      else {
        val typeArgString = typeArgsDescs.map(f).mkString(", ")
        s"$className[$typeArgString]"
      }
    }

    def liftable: Liftable[_, A] =
      !!!(s"Cannot get Liftable instance for $this")

    final lazy val sourceType: RType[_] = liftable.sourceType
    protected def collectMethods: Map[Method, MethodDesc] = Map()
    protected lazy val methods: Map[Method, MethodDesc] = collectMethods

    def invokeUnlifted(mc: MethodCall, dataEnv: DataEnv): AnyRef = {
      val res = methods.get(mc.method) match {
        case Some(WMethodDesc(wrapSpec, method)) =>
          val srcArgs = getSourceValues(dataEnv, true, mc.receiver +: mc.args:_*)
          def msg = s"Cannot invoke method $method on object $wrapSpec with arguments $srcArgs"
          val res =
            try method.invoke(wrapSpec, srcArgs:_*)
            catch {
              case e: InvocationTargetException => !!!(msg, e.getTargetException)
              case t: Throwable => !!!(msg, t)
            }
          res
        case Some(RMethodDesc(method)) =>
          val srcObj = getSourceValues(dataEnv, false, mc.receiver).head
          val srcArgs = getSourceValues(dataEnv, false, mc.args:_*)
          def msg = s"Cannot invoke method $method on object $srcObj with arguments $srcArgs"
          val res =
            try method.invoke(srcObj, srcArgs:_*)
            catch {
              case e: InvocationTargetException => !!!(msg, e.getTargetException)
              case t: Throwable => !!!(msg, t)
            }
          res
        case None =>
          !!!(s"Cannot perform unliftedInvoke of $mc")
      }
      // this if is required because res == null in case of Unit return type
      if (mc.selfType == UnitElement) ().asInstanceOf[AnyRef] else res
    }

    def <:<(e: Elem[_]) = e.getClass.isAssignableFrom(this.getClass)
    def >:>(e: Elem[_]) = e <:< this
  }

  object Elem {
    implicit def rtypeToElem[SA, A](tSA: RType[SA])(implicit lA: Liftables.Liftable[SA,A]): Elem[A] = lA.eW

    def unapply[T, E <: Elem[T]](s: Rep[T]): Option[E] = Some(s.elem.asInstanceOf[E])

    def pairify(es: Iterator[Elem[_]]): Elem[_] = {
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

    def methodKey(m: Method) = {
      val ann = m.getDeclaredAnnotation(classOf[OverloadId])
      if (ann != null)
        s"${m.getName}_${ann.value}"
      else
        m.getName
    }

    def declaredMethods(cls: Class[_], srcCls: Class[_], methodNames: Set[String]): Seq[(Method, MethodDesc)] = {
      val rmethods = cls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val smethods = srcCls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val mapping = CollectionUtil.joinSeqs(rmethods, smethods)(methodKey, methodKey)
      mapping.map { case (rm, sm) =>
        (rm, RMethodDesc(sm))
      }.to[Seq]
    }

    def declaredWrapperMethods(wrapSpec: WrapSpec, wcls: Class[_], methodNames: Set[String]): Seq[(Method, MethodDesc)] = {
      val specCls = wrapSpec.getClass
      val wMethods = wcls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val specMethods = specCls.getDeclaredMethods.filter(m => methodNames.contains(m.getName))
      val mapping = CollectionUtil.joinSeqs(wMethods, specMethods)(methodKey, methodKey)
      mapping.map { case (wm, sm) =>
        (wm, WMethodDesc(wrapSpec, sm))
      }.to[Seq]
    }

  }

  def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): AnyRef =
    e.invokeUnlifted(mc, dataEnv)

//  private lazy val debug$ElementCounter = counter[Elem[_]]

  private[scalan] def getConstructor(clazz: Class[_]) = {
    val constructors = clazz.getDeclaredConstructors()
    if (constructors.length != 1)
      !!!(s"Element class $clazz has ${constructors.length} constructors, 1 expected")
    else
      constructors(0)
  }

  // FIXME See https://github.com/scalan/scalan/issues/252
  def cachedElem[E <: Elem[_]](args: AnyRef*)(implicit tag: ClassTag[E]) = {
    cachedElem0(tag.runtimeClass, None, args).asInstanceOf[E]
  }

  def cachedElemByClass[E <: Elem[_]](args: AnyRef*)(implicit clazz: Class[E]) = {
    cachedElem0(clazz, None, args).asInstanceOf[E]
  }

  private def cachedElemI(clazz: Class[_], argsIterator: Iterator[TypeDesc]) = {
    val constructor = getConstructor(clazz)
    // -1 because the constructor includes `self` argument, see cachedElem0 below
    val args = argsIterator.take(constructor.getParameterTypes.length - 1).toSeq
    cachedElem0(clazz, Some(constructor), args)
  }

  protected val elemCache = AVHashMap[(Class[_], Seq[AnyRef]), AnyRef](1000)

  // TODO optimize: avoid tuple key in map since Tuple2.hashCode and equals takes 95% of lookup time
  private def cachedElem0(clazz: Class[_], optConstructor: Option[java.lang.reflect.Constructor[_]], args: Seq[AnyRef]) = {
    elemCache.getOrElseUpdate(
      (clazz, args), {
        val constructor = if (optConstructor.isEmpty) getConstructor(clazz) else optConstructor.get
        val ownerType = getOwnerParameterType(constructor)
        val constructorArgs = addOwnerParameter(ownerType, args)
        constructor.newInstance(constructorArgs: _*).asInstanceOf[AnyRef]
      }).asInstanceOf[Elem[_]]
  }

  def cleanUpTypeName(tpe: Type) = tpe.toString.
    replaceAll("[A-Za-z0-9_.]*this.", "").
    replace("scala.math.Numeric$", "").
    replace("scala.math.Ordering$", "").
    replace("scala.", "").
    replace("java.lang.", "").
    replaceAll("""[^# \[\],>]*[#$]""", "")

  def element[A](implicit ea: Elem[A]): Elem[A] = ea

  abstract class BaseElem[A](defaultValue: A)(implicit val tag: WeakTypeTag[A]) extends Elem[A] with Serializable with scala.Equals

  class BaseElemLiftable[A](defaultValue: A, val tA: RType[A])(implicit tag: WeakTypeTag[A]) extends BaseElem[A](defaultValue) {
    override def buildTypeArgs = EmptyTypeArgs
    override val liftable = new Liftables.BaseLiftable[A]()(this, tA)
    override def canEqual(other: Any) = other.isInstanceOf[BaseElemLiftable[_]]
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: BaseElemLiftable[_] => tA == other.tA
      case _ => false
    })
    override val hashCode = tA.hashCode
  }

  case class PairElem[A, B](eFst: Elem[A], eSnd: Elem[B]) extends Elem[(A, B)] {
    assert(eFst != null && eSnd != null)
    lazy val tag = {
      implicit val tA = eFst.tag
      implicit val tB = eSnd.tag
      weakTypeTag[(A, B)]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eFst)}, ${f(eSnd)})"
    override def buildTypeArgs = ListMap("A" -> (eFst -> Covariant), "B" -> (eSnd -> Covariant))
    override def liftable: Liftables.Liftable[_, (A, B)] =
      Liftables.asLiftable[(_,_), (A,B)](Liftables.PairIsLiftable(eFst.liftable, eSnd.liftable))
  }

  case class SumElem[A, B](eLeft: Elem[A], eRight: Elem[B]) extends Elem[A | B] {
    lazy val tag = {
      implicit val tA = eLeft.tag
      implicit val tB = eRight.tag
      weakTypeTag[A | B]
    }
    override def getName(f: TypeDesc => String) = s"(${f(eLeft)} | ${f(eRight)})"
    override def buildTypeArgs = ListMap("A" -> (eLeft -> Covariant), "B" -> (eRight -> Covariant))
  }

  case class FuncElem[A, B](eDom: Elem[A], eRange: Elem[B]) extends Elem[A => B] {
    import Liftables._
    lazy val tag = {
      implicit val tA = eDom.tag
      implicit val tB = eRange.tag
      weakTypeTag[A => B]
    }
    override def getName(f: TypeDesc => String) = s"${f(eDom)} => ${f(eRange)}"
    override def buildTypeArgs = ListMap("A" -> (eDom -> Contravariant), "B" -> (eRange -> Covariant))
    override def liftable: Liftable[_, A => B] =
      asLiftable[_ => _, A => B](FuncIsLiftable(eDom.liftable, eRange.liftable))
  }


  val AnyElement: Elem[Any] = new BaseElemLiftable[Any](null, AnyType)

  val LazyAnyElement = Lazy(AnyElement)

  val AnyRefElement: Elem[AnyRef] = new BaseElemLiftable[AnyRef](null, AnyRefType)

  // somewhat ugly casts, but they completely disappear after type erasure
  // (so not present in Java bytecode)
  val NothingElement: Elem[Nothing] =
    new BaseElemLiftable[Null](
      null, NothingType.asInstanceOf[RType[Null]]
    )(weakTypeTag[Nothing].asInstanceOf[WeakTypeTag[Null]]).asElem[Nothing]

  implicit val BooleanElement: Elem[Boolean] = new BaseElemLiftable(false, BooleanType)
  implicit val ByteElement: Elem[Byte] = new BaseElemLiftable(0.toByte, ByteType)
  implicit val ShortElement: Elem[Short] = new BaseElemLiftable(0.toShort, ShortType)
  implicit val IntElement: Elem[Int] = new BaseElemLiftable(0, IntType)
  implicit val LongElement: Elem[Long] = new BaseElemLiftable(0L, LongType)
  implicit val FloatElement: Elem[Float] = new BaseElemLiftable(0.0F, FloatType)
  implicit val DoubleElement: Elem[Double] = new BaseElemLiftable(0.0, DoubleType)
  implicit val UnitElement: Elem[Unit] = new BaseElemLiftable((), UnitType)
  implicit val StringElement: Elem[String] = new BaseElemLiftable("", StringType)
  implicit val CharElement: Elem[Char] = new BaseElemLiftable('\u0000', CharType)

  implicit def pairElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[(A, B)] =
    cachedElemByClass[PairElem[A, B]](ea, eb)(classOf[PairElem[A, B]])
  implicit def sumElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A | B] =
    cachedElemByClass[SumElem[A, B]](ea, eb)(classOf[SumElem[A, B]])
  implicit def funcElement[A, B](implicit ea: Elem[A], eb: Elem[B]): Elem[A => B] =
    cachedElemByClass[FuncElem[A, B]](ea, eb)(classOf[FuncElem[A, B]])
  ///implicit def elemElement[A](implicit ea: Elem[A]): Elem[Elem[A]]

  implicit def PairElemExtensions[A, B](eAB: Elem[(A, B)]): PairElem[A, B] = eAB.asInstanceOf[PairElem[A, B]]
  implicit def SumElemExtensions[A, B](eAB: Elem[A | B]): SumElem[A, B] = eAB.asInstanceOf[SumElem[A, B]]
  implicit def FuncElemExtensions[A, B](eAB: Elem[A => B]): FuncElem[A, B] = eAB.asInstanceOf[FuncElem[A, B]]
  //  implicit def ElemElemExtensions[A](eeA: Elem[Elem[A]]): ElemElem[A] = eeA.asInstanceOf[ElemElem[A]]

  implicit def toLazyElem[A](implicit eA: Elem[A]): LElem[A] = Lazy(eA)

  val EmptyTypeArgs: ListMap[String, (TypeDesc, Variance)] = ListMap.empty

  def TypeArgs(descs: (String, (TypeDesc, Variance))*) = ListMap(descs: _*)

  // can be removed and replaced with assert(value.elem == elem) after #72
  def assertElem(value: Rep[_], elem: Elem[_]): Unit = assertElem(value, elem, "")
  def assertElem(value: Rep[_], elem: Elem[_], hint: => String): Unit = {
    assert(value.elem == elem,
      s"${value.toStringWithType} doesn't have type ${elem.name}" + (if (hint.isEmpty) "" else s"; $hint"))
  }
  def assertEqualElems[A](e1: Elem[A], e2: Elem[A], m: => String): Unit =
    assert(e1 == e2, s"Element $e1 != $e2: $m")

  def withElemOf[A, R](x: Rep[A])(block: Elem[A] => R): R = block(x.elem)
  def withResultElem[A, B, R](f: Rep[A => B])(block: Elem[B] => R): R = block(withElemOf(f) { e => e.eRange })

  @implicitNotFound(msg = "No Cont available for ${F}.")
  trait Cont[F[_]] extends TypeDesc {
    def tag[T](implicit tT: WeakTypeTag[T]): WeakTypeTag[F[T]]
    def lift[T](implicit eT: Elem[T]): Elem[F[T]]
    def unlift[T](implicit eFT: Elem[F[T]]): Elem[T]
    def getElem[T](fa: Rep[F[T]]): Elem[F[T]]
    def getItemElem[T](fa: Rep[F[T]]): Elem[T] = unlift(getElem(fa))
    def unapply[T](e: Elem[_]): Option[Elem[F[T]]]

    def getName(f: TypeDesc => String): String = {
      // note: will use WeakTypeTag[x], so x type parameter ends up in the result
      // instead of the actual type parameter it's called with (see below)
      def tpeA[x] = tag[x].tpe

      val tpe = tpeA[Nothing]

      val str = cleanUpTypeName(tpe)

      if (str.endsWith("[x]"))
        str.stripSuffix("[x]")
      else
        "[x]" + str
    }
    def isFunctor = this.isInstanceOf[Functor[F]]
  }

  def container[F[_]: Cont] = implicitly[Cont[F]]

  implicit def containerElem[F[_]:Cont, A:Elem]: Elem[F[A]] = container[F].lift(element[A])

  trait Functor[F[_]] extends Cont[F] {
    def map[A,B](a: Rep[F[A]])(f: Rep[A] => Rep[B]): Rep[F[B]]
  }
}
