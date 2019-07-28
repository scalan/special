package scalan.meta

import com.trueaccord.lenses.Updatable

import scala.reflect.internal.ModifierFlags
import java.util.Objects

import scala.collection.immutable.{HashMap, HashSet}
import scalan.{ArgList, Constructor, ContainerType, FunctorType, Liftable, Reified, NeverInline, External, Convertible}
import scalan.meta.Symbols._
import scalan.meta.ScalanAstTransformers.{TypeNameCollector, SubstTypeTransformer, TypeTransformerInAst}

import scala.collection.{mutable, GenIterable}
import com.typesafe.config.ConfigUtil
import scalan.util.{Covariant, Contravariant, PrintExtensions, Invariant}
import PrintExtensions._
import ScalanAstExtensions._
import scalan.util.CollectionUtil._

object ScalanAst {

  // STpe universe --------------------------------------------------------------------------

  sealed abstract class STpeExpr {
    def name: String

    def args: List[STpeExpr] = Nil
  }

  type STpeExprs = List[STpeExpr]
  type STpeSubst = Map[String, STpeExpr]

  trait HasUnderlying { this: STpeExpr =>
    def underlying: STpeExpr
  }

  /** Represents scala.reflect.internal.Types.NoType | NoPrefix */
  case class STpeEmpty() extends STpeExpr {
    def name = "Empty"
  }

  /** Represents scala.reflect.internal.Types.ConstantType */
  case class STpeConst(const: SConst, underlying: STpeExpr) extends STpeExpr with HasUnderlying {
    def name = "Constant"
  }

  /** A class for this-types of the form <sym>.this.type */
  case class STpeThis(fullNameString: String, underlying: STpeExpr) extends STpeExpr with HasUnderlying {
    def name = s"$fullNameString.this.type"
  }

  /** <pre>.<single>.type */
  case class STpeSingle(pre: STpeExpr, name: String, underlying: STpeExpr) extends STpeExpr with HasUnderlying

  /** Invocation of a trait with arguments */
  case class STraitCall(name: String, override val args: List[STpeExpr] = Nil) extends STpeExpr {
    override def toString = name + args.asTypeParams()
    def isDef = name == "Def"
    def toTypeApply = STypeApply(this, Nil)
  }

  case class STpePrimitive(val name: String, defaultValueString: String) extends STpeExpr {
    override def toString = name
  }

  case class STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) extends STpeExpr {
    override def name = "Bounds"

    override def toString = ">:" + lo + "<:" + hi
  }

  val TpeAny = STpePrimitive("Any", "AnyElement.defaultRepValue")
  val TpeAnyRef = STpePrimitive("AnyRef", "AnyRefElement.defaultRepValue")
  val TpeUnit = STpePrimitive("Unit", "()")
  val TpeShort = STpePrimitive("Short", "0")
  val TpeInt = STpePrimitive("Int", "0")
  val TpeLong = STpePrimitive("Long", "0l")
  val TpeByte = STpePrimitive("Byte", "0.toByte")
  val TpeBoolean = STpePrimitive("Boolean", "false")
  val TpeFloat = STpePrimitive("Float", "0.0f")
  val TpeDouble = STpePrimitive("Double", "0.0")
  val TpeString = STpePrimitive("String", "\"\"")
  val TpeChar = STpePrimitive("Char", "0.toChar")
  val TpeNothing = STpePrimitive("Nothing", "???")

  val STpePrimitives = Map(
    "Any" -> TpeAny,
    "AnyRef" -> TpeAnyRef,
    "Nothing" -> TpeNothing,
    "Unit" -> TpeUnit,
    "Short" -> TpeShort,
    "Int" -> TpeInt,
    "Long" -> TpeLong,
    "Byte" -> TpeByte,
    "Boolean" -> TpeBoolean,
    "Float" -> TpeFloat,
    "Double" -> TpeDouble,
    "String" -> TpeString,
    "Char" -> TpeChar
  )

  case class STpeTuple(override val args: List[STpeExpr]) extends STpeExpr {
    def name = "Tuple" + args.length

    override def toString = args.mkString("(", ", ", ")")
  }

  case class STpeStruct(fields: List[(String, STpeExpr)]) extends STpeExpr {
    def name = "Struct"

    override def toString = fields.map { case (n, t) => s"$n: $t" }.mkString("{", "; ", "}")
  }

  case class STpeFunc(domain: STpeExpr, range: STpeExpr) extends STpeExpr {
    def name = "Function1"

    override def args = List(domain, range)

    override def toString = {
      val domainStr = domain match {
        case tuple: STpeTuple => s"($tuple)"
        case _ => domain.toString
      }
      s"$domainStr => $range"
    }
  }

  def createSubst(args: STpeArgs, types: List[STpeExpr]): STpeSubst =
    args.map(_.name).zip(types).toMap

  implicit class STpeExprOps(self: STpeExpr) {
    def toIdentifier: String = {
      def mkId(name: String, parts: Seq[STpeExpr]) =
        (name +: parts).mkString("_")

      self match {
        case STpePrimitive(name, _) => name
        case STraitCall(name, args) => mkId(name, args)
        case STpeTuple(items) => mkId("Tuple", items)
        //case STpeSum(items) => mkId("Sum", items)
        case STpeFunc(domain, range) => mkId("Func", Seq(domain, range))
        case STpeTypeBounds(lo, hi) => mkId("Bounds", Seq(lo, hi))
        case _ => self.name
      }
    }

    def applySubst(subst: STpeSubst): STpeExpr = {
      val trans = new SubstTypeTransformer(subst)
      val res = trans(self)
      res
    }

    def unRep(module: SUnitDef, isVirtualized: Boolean): Option[STpeExpr] = self match {
      case t if !isVirtualized => Some(t)
      case STraitCall("Elem", Seq(t)) =>  // Elem[t] --> tpe
        Some(self)
      case module.context.RepTypeOf(t) => Some(t)
      case _ => None
    }

    def isRep(module: SUnitDef, isVirtualized: Boolean) = unRep(module, isVirtualized) match {
      case Some(_) => true
      case None => false
    }

    def isTupledFunc = self match {
      case STraitCall("Rep", List(STpeFunc(STpeTuple(a1 :: a2 :: tail), _))) => true
      case STpeFunc(STpeTuple(a1 :: a2 :: tail), _) => true
      case _ => false
    }

    def names: Set[String] = {
      val names = mutable.HashSet[String]()
      val collector = new TypeNameCollector(names)
      collector(self)
      names.toSet
    }
  }

  case class STpeSingleton(ref: SExpr) extends STpeExpr {
    def name = "Singleton"
  }

  case class STpeSelectFromTT(qualifier: STpeExpr, tname: String) extends STpeExpr {
    def name = "SelectFromTypeTree"
  }

  case class STpeAnnotated(tpt: STpeExpr, annot: String) extends STpeExpr with HasUnderlying {
    def name = "Annotated" + tpt.name

    override def underlying: STpeExpr = tpt

    override def toString = tpt.toString + " @" + annot
  }

  case class STpeExistential(tpt: STpeExpr, items: List[SBodyItem]) extends STpeExpr with HasUnderlying {
    def name = "Existential"

    override def underlying: STpeExpr = tpt

    override def toString = {
      val body = items map (_.toString)
      s"$tpt forSome {${body.mkString(";")}}"
    }
  }

  case class STpeBind(tname: String, texpr: STpeExpr) extends STpeExpr {
    def name = "TypedBind"
  }

  case class STpeCompound(parents: List[STpeExpr], items: List[SBodyItem]) extends STpeExpr {
    def name = "Compound Type Tree"
  }

  case class STpeMethod(tparams: List[String], paramSections: List[List[STpeExpr]], resultType: STpeExpr) extends STpeExpr {
    lazy val params = paramSections.flatten

    def name = (if (tparams.nonEmpty) tparams.mkString("[", ",", "]") else "") +
      paramSections.map(_.mkString("(", ",", ")")).mkString("") +
      ": " + resultType
  }

  // TpePath universe ------------------------------------------------------------------------------
  sealed abstract class STpePath {
  }

  case object SNilPath extends STpePath

  sealed abstract class SBasedPath extends STpePath {
    def base: STpeExpr
  }

  case class STuplePath(base: STpeExpr, index: Int, tail: STpePath) extends SBasedPath

  case class SDomPath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SRangePath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SThunkPath(base: STpeExpr, tail: STpePath) extends SBasedPath

  case class SStructPath(base: STpeExpr, fieldName: String, tail: STpePath) extends SBasedPath

  case class SEntityPath(base: STpeExpr, entity: SEntityDef, tyArg: STpeArg, tail: STpePath) extends SBasedPath

  object STpePath {
    def findInEntity(e: SEntityDef, tc: STraitCall, argName: String)
                    (implicit context: AstContextBase): Option[STpePath] = {
      val args = tc.args
      for (i <- args.indices) {
        find(args(i), argName) match {
          case Some(tailPath) =>
            return Some(SEntityPath(tc, e, e.tpeArgs(i), tailPath))
          case None =>
        }
      }
      None
    }

    def find(tpe: STpeExpr, argName: String)(implicit context: AstContextBase): Option[STpePath] = tpe match {
      case STpePrimitive(_, _) => None
      case STpeFunc(d, r) =>
        find(d, argName) match {
          case Some(tailPath) => Some(SDomPath(tpe, tailPath))
          case None => find(r, argName) match {
            case Some(tailPath) => Some(SRangePath(tpe, tailPath))
            case None => None
          }
        }
      case t@STpeTuple(_) =>
        def findInTuple(t: STpeTuple): Option[STpePath] = {
          for ((item, i) <- t.args.zipWithIndex) {
            find(item, argName) match {
              case Some(tailPath) =>
                return Some(STuplePath(t, i, tailPath))
              case None =>
            }
          }
          None
        }
        findInTuple(t)
      case STraitCall("Rep", List(tT)) =>
        find(tT, argName)
      case STraitCall("Thunk", List(tT)) =>
        find(tT, argName).map(tail => SThunkPath(tpe, tail))
      case context.TypeDef(_, context.RepTypeOf(STraitCall(en @ context.Entity(_, e), args))) =>
        findInEntity(e, STraitCall(en, args), argName)
      case s@STpeStruct(_) =>
        def findInStruct(s: STpeStruct): Option[STpePath] = {
          for ((fn, ft) <- s.fields) {
            find(ft, argName) match {
              case Some(tailPath) =>
                return Some(SStructPath(s, fn, tailPath))
              case None =>
            }
          }
          None
        }
        findInStruct(s)
      case STraitCall(`argName`, Nil) =>
        Some(SNilPath)
      case tc@STraitCall(context.Entity(_, e), args) =>
        findInEntity(e, tc, argName)
      case _ => None
    }
  }

  // SAnnotation universe --------------------------------------------------------------------------
  trait SAnnotation {
    def annotationClass: String
    def tpeArgs: List[STpeExpr]
    def args: List[SExpr]
  }

  /** Annotation that can be attached to any STmplDef */
  case class SEntityAnnotation(
      annotationClass: String,
      tpeArgs: List[STpeExpr],
      args: List[SExpr]) extends SAnnotation

  /** Annotation that can be attached to any SMethodDef */
  case class SMethodAnnotation(
      annotationClass: String,
      tpeArgs: List[STpeExpr],
      args: List[SExpr]) extends SAnnotation

  case class SArgAnnotation(
      annotationClass: String,
      tpeArgs: List[STpeExpr],
      args: List[SExpr]) extends SAnnotation

  case class STypeArgAnnotation(
      annotationClass: String,
      tpeArgs: List[STpeExpr],
      args: List[SExpr]) extends SAnnotation

  final val EntityAnnotation         = classOf[scalan.Entity].getSimpleName
  final val LiftableAnnotation       = classOf[Liftable].getSimpleName
  final val ConvertibleAnnotation    = classOf[Convertible].getSimpleName
  final val ConstructorAnnotation    = classOf[Constructor].getSimpleName
  final val ExternalAnnotation       = classOf[External].getSimpleName
  final val ArgListAnnotation        = classOf[ArgList].getSimpleName
  final val ContainerTypeAnnotation  = classOf[ContainerType].getSimpleName
  final val FunctorTypeAnnotation    = classOf[FunctorType].getSimpleName
  final val ReifiedTypeArgAnnotation = classOf[Reified].getSimpleName
  final val NeverInlineAnnotation    = classOf[NeverInline].getSimpleName
  final val SpecializedAnnotation    = classOf[specialized].getSimpleName
  final val InlineAnnotation         = classOf[inline].getSimpleName

  val AnnotationsToRemove = Array(SpecializedAnnotation, InlineAnnotation)

  object ExternalEntityAnnotation {
    def apply(externalName: String) =
      SEntityAnnotation(ExternalAnnotation, Nil, List(SConst(externalName,Some(TpeString))))

    def unapply(a: SEntityAnnotation): Option[String] = a match {
      case SEntityAnnotation(ExternalAnnotation, _, List(SConst(externalName,_))) =>
        Some(externalName.toString)
      case _ => None
    }
  }

  // SExpr universe --------------------------------------------------------------------------
  trait SExpr {
    def exprType: Option[STpeExpr] = None
  }

  case class SEmpty(override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SConst(c: Any,
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SIdent(name: String,
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SAssign(left: SExpr, right: SExpr,
                     override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SArgSection(args: List[SExpr]) {
//    def hasThunkArg: Boolean = args.exists {}
  }

  implicit def toArgSection(args: List[SExpr]): SArgSection = SArgSection(args)
  implicit def fromArgSection(section: SArgSection): List[SExpr] = section.args

  case class SApply(fun: SExpr, ts: List[STpeExpr],
                    argss: List[SArgSection],
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SExprApply(fun: SExpr, ts: List[STpeExpr],
                        override val exprType: Option[STpeExpr] = None) extends SExpr

  case class STypeApply(tpe: STraitCall, ts: List[SExpr] = Nil) extends SExpr {
    override val exprType: Option[STpeExpr] = Some(tpe)
  }

  case class SSelect(expr: SExpr, tname: String,
                     override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SBlock(init: List[SExpr], last: SExpr,
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SIf(cond: SExpr, th: SExpr, el: SExpr,
                 override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SAscr(expr: SExpr, pt: STpeExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SFunc(params: List[SValDef], res: SExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SConstr(name: String, args: List[SExpr],
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SThis(typeName: String,
                   override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SSuper(name: String, qual: String, field: String,
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SAnnotated(expr: SExpr, annot: String, override val exprType: Option[STpeExpr] = None) extends SExpr

  case class STuple(exprs: List[SExpr], override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SCase(pat: SPattern, guard: SExpr, body: SExpr,
                   override val exprType: Option[STpeExpr] = None) extends SExpr

  case class SMatch(selector: SExpr, cases: List[SCase],
                    override val exprType: Option[STpeExpr] = None) extends SExpr

  trait SPattern

  case class SWildcardPattern() extends SPattern

  case class SLiteralPattern(const: SConst) extends SPattern

  case class SStableIdPattern(id: SIdent) extends SPattern

  case class SSelPattern(sel: SExpr, name: String) extends SPattern

  case class SAltPattern(alts: List[SPattern]) extends SPattern

  case class STypedPattern(tpe: STpeExpr) extends SPattern

  case class SBindPattern(name: String, pat: SPattern) extends SPattern

  case class SApplyPattern(fun: SExpr, pats: List[SPattern]) extends SPattern

  // SBodyItem universe ----------------------------------------------------------------------
  abstract class SBodyItem extends SExpr {
    def signature: DefSig
  }

  case class SImportStat(name: String, inCake: Boolean = false) extends SBodyItem {
    override def signature = DefSig(DefType.Import, name, Nil)
  }

  case class SMethodDef(
      owner: SSymbol,
      name: String, tpeArgs: STpeArgs,
      argSections: List[SMethodArgs],
      tpeRes: Option[STpeExpr],
      isImplicit: Boolean,
      isOverride: Boolean,
      overloadId: Option[String],
      annotations: List[SMethodAnnotation] = Nil,
      body: Option[SExpr] = None,
      isTypeDesc: Boolean = false) extends SBodyItem with SEntityItem with Updatable[SMethodDef] {
    val symbol = SEntityItemSymbol(owner, name, DefType.Def)
    def isMonomorphic = tpeArgs.isEmpty
    override def isAbstract: Boolean = body.isEmpty
    def isNeverInline: Boolean = hasAnnotation(NeverInlineAnnotation)
    override def argss: List[List[SMethodOrClassArg]] = argSections.map(_.args)
    override def rhs: Option[SExpr] = body
    override def exprType = ??? // TODO build STpeFunc for this method type
    def externalOpt: Option[SMethodAnnotation] = annotations.find(_.annotationClass == "External")

    def getAnnotation(annotName: String): Option[SMethodAnnotation] = annotations.find(a => a.annotationClass == annotName)
    def hasAnnotation(annotName: String): Boolean = getAnnotation(annotName).isDefined

//    def isExtractableArg(module: SModuleDef, tpeArg: STpeArg): Boolean = {
//      allArgs.exists(a => STpePath.find(module, a.tpe, tpeArg.name).isDefined)
//    }

    def explicitArgs: List[SMethodArg] = argSections.flatMap(_.args.filterNot(_.impFlag))
    def implicitArgs: List[SMethodArg] = argSections.flatMap(_.args.filter(_.impFlag))

    def allArgs = argSections.flatMap(_.args)

    def getOriginal: Option[SMethodDef] = {
      annotations.collectFirst {
        case annot @ SMethodAnnotation("Constructor", _, _) => annot.args collectFirst {
          case SAssign(SIdent("original", _), origMethod: SMethodDef, _) => origMethod
        }
      }.flatten
    }

    def cleanedArgs: List[SMethodArgs] = getOriginal match {
      case Some(method) =>
        def existsClassTag(tpeArgs: List[STpeExpr]): Boolean = {
          val relatedClassTag = (getOriginal map (_.argSections map (_.args))).toList.flatten.flatten collectFirst {
            case marg@SMethodArg(_, _, _, STraitCall("ClassTag", origTpeArgs), _, _, _) if origTpeArgs == tpeArgs => marg
          }
          !relatedClassTag.isEmpty
        }

        def isAdded(arg: SMethodArg): Boolean = arg match {
          case SMethodArg(_, _, _, STraitCall("Elem" | "Cont", tpeArgs), _, _, _) => !existsClassTag(tpeArgs)
          case _ => false
        }

        val (currImp, currNonImp) = argSections.splitArgSections()

        val newCurrImp = currImp map { s => s.copy(args = s.args.filterNot(isAdded(_))) } filter {
          !_.args.isEmpty
        }
        currNonImp ++ newCurrImp
      case None => argSections
    }

    def signature: DefSig = {
      DefSig(DefType.Def, name, argSections.map(sec => sec.args.map(a => a.tpe)))
    }
  }

  object SMethodDef {
    def emptyArgSection = List(SMethodArgs(Nil))
  }

  case class DefSig(defType: DefType.Value, name: String, argTypes: List[List[STpeExpr]])

  case class SValDef(
      owner: SSymbol,
      name: String,
      tpe: Option[STpeExpr],
      isLazy: Boolean,
      isImplicit: Boolean,
      expr: SExpr,
      isAbstract: Boolean = false,
      annotations: List[SArgAnnotation] = Nil,
      isTypeDesc: Boolean = false
  ) extends SBodyItem with SEntityItem {
    val symbol = SEntityItemSymbol(owner, name, DefType.Val)
    override def signature = DefSig(DefType.Val, name, Nil)
    override def tpeArgs: STpeArgs = Nil
    override def argss: List[List[SMethodOrClassArg]] = Nil
    override def tpeRes = tpe
    override def exprType = tpe
    override def rhs: Option[SExpr] = Some(expr)
  }

  case class STpeDef(
      owner: SSymbol,
      name: String,
      tpeArgs: STpeArgs,
      tpe: STpeExpr,
      isAbstract: Boolean = false,
      annotations: List[SArgAnnotation] = Nil
  ) extends SBodyItem with SEntityItem {
    val symbol = SEntityItemSymbol(owner, name, DefType.Type)
    def signature = DefSig(DefType.Type, name, Nil)
    def rhs: Option[SExpr] = None
    def isImplicit: Boolean = false
    def argss: List[List[SMethodOrClassArg]] = Nil
    def tpeRes: Option[STpeExpr] = Some(tpe)
    def isTypeDesc: Boolean = false
    override def toString = s"type $name${tpeArgs.opt(as => s"[${as.rep()}]")} = $tpe"
  }

  case class STpeArg(
      name: String,
      bound: Option[STpeExpr] = None,
      contextBound: List[String] = Nil,
      tparams: List[STpeArg] = Nil,
      flags: Long = ModifierFlags.PARAM,
      annotations: List[STypeArgAnnotation] = Nil) {
    def isHighKind = tparams.nonEmpty
    def classOrMethodArgName(n: String = name): String = if (isHighKind) "c" + n else "e" + n
    def descName: String = if (isHighKind) "Cont" else "Elem"

    val variance =
      if (hasFlag(ModifierFlags.COVARIANT))
        Covariant
      else if (hasFlag(ModifierFlags.CONTRAVARIANT))
        Contravariant
      else
        Invariant

    def isCovariant = variance == Covariant

    def hasFlag(flag: Long) = (flag & flags) != 0L

    def declaration: String =
      if (isHighKind) {
        val params = tparams.rep(_.declaration)
        s"$name[$params]"
      }
      else name + bound.opt(b => s" <: ${b.name}")

    def toTraitCall: STraitCall = {
      val args = if (tparams.forall(p => p.name == "_")) Nil
                 else tparams.map { p => p.toTraitCall }
      STraitCall(name, args)
    }

    def getArgBounds(args: List[SMethodArgs]): List[STraitCall] = {
      args.lastOption match {
        case Some(SMethodArgs(lastArgs)) =>
          lastArgs.collect {
            case SMethodArg(true, _, _, b@STraitCall(_, List(STraitCall(tname, _))), _, _, _) if tname == name => b
          }
        case None => Nil
      }
    }

    def hasElemBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "Elem")

    def hasContBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "Cont")

    def hasWeakTypeTagBound(args: List[SMethodArgs]) = getArgBounds(args) exists (_.name == "WeakTypeTag")
  }

  type STpeArgs = List[STpeArg]

  trait SMethodOrClassArg {
    def impFlag: Boolean
    def overFlag: Boolean
    def name: String
    def tpe: STpeExpr
    def default: Option[SExpr]
    def annotations: List[SArgAnnotation]
    def isArgList = annotations.exists(a => a.annotationClass == ArgListAnnotation)
    def isTypeDesc: Boolean
  }
  object SMethodOrClassArg {
  }
  case class SMethodArg(
                         impFlag: Boolean,
                         overFlag: Boolean,
                         name: String,
                         tpe: STpeExpr,
                         default: Option[SExpr],
                         annotations: List[SArgAnnotation] = Nil,
                         isTypeDesc: Boolean = false)
    extends SMethodOrClassArg

  def getUniqueName(name: String, env: Set[String]): String = {
    var n = 0
    var newName = name
    while (env.contains(newName)) {
      n += 1
      newName = name + n
    }
    return newName
  }

  def disambiguateNames(names: List[String], entitySubst: STpeSubst): STpeSubst = {
    var contextNames = HashSet[String]()
    for ((a, t) <- entitySubst) {
      contextNames += a
      contextNames ++= t.names
    }
    var nameSubst = HashMap[String, STpeExpr]()
    for (name <- names) {
      if (contextNames.contains(name)) {
        val newName = getUniqueName(name, contextNames)
        contextNames += newName
        nameSubst += (name -> STraitCall(newName))
      }
    }
    nameSubst
  }

  trait SEntityItem extends SExpr with NamedDef {
    def annotations: List[SAnnotation]
    def isImplicit: Boolean
    def isAbstract: Boolean
    def name: String
    def tpeArgs: STpeArgs
    def argss: List[List[SMethodOrClassArg]]
    def tpeRes: Option[STpeExpr]
    def rhs: Option[SExpr]
    def isTypeDesc: Boolean
    def hasNoArgs = argss.flatten.isEmpty
    def isMethod: Boolean = this.isInstanceOf[SMethodDef]
    def applySubst(subst: STpeSubst)(implicit ctx: AstContextBase): SEntityItem = {
      val typeTrans = new SubstTypeTransformer(subst)
      val trans = new TypeTransformerInAst(typeTrans)
      trans(this).asInstanceOf[SEntityItem]
    }
  }

  case class SEntityMember(entity: SEntityDef, item: SEntityItem) {
    def isMethod: Boolean = item.isMethod
    def combinedTpeArgs = entity.tpeArgs ++ item.tpeArgs
    def matches(other: SEntityMember)(implicit ctx: AstContextBase): Boolean = {
      item.name == other.item.name && ((item, other.item) match {
        case (m1: SMethodDef, m2: SMethodDef) =>
          val okArgsNum = m1.allArgs.length == m2.allArgs.length
          if (m1.isMonomorphic && m2.isMonomorphic) {
            // both monomorphic
            val okEqualArgs = okArgsNum && m1.allArgs.zip(m2.allArgs).forall { case (a1, a2) => a1.tpe == a2.tpe }
            okEqualArgs
          } else if (!(m1.isMonomorphic || m2.isMonomorphic)) {
            // both polymorphic
            val okTyArgNum = m1.tpeArgs.length == m2.tpeArgs.length
            if (okTyArgNum && okArgsNum) {
              val lin = entity.linearizationWithSubst(Map())
              val entSubst = lin.collectFirst {
                case (e, args) if e.name == other.entity.name => e.createTpeSubst(args)
              }.get
              val subst = createSubst(m2.tpeArgs, m1.tpeArgs.map(_.toTraitCall))
              val okEqualArgs = m1.allArgs.zip(m2.allArgs).forall { case (a1, a2) => a1.tpe == a2.tpe.applySubst(entSubst ++ subst) }
              okEqualArgs
            }
            else false // different args
          } else
            false // one mono another polymorphic
        case (m1: SMethodDef, _) => false   // def cannot override val
        case (_, m2: SMethodDef) => m2.hasNoArgs // val can override def
        case _ => true  // they are both vals with the same name
      })
    }
  }

  case class SClassArg(
      owner: SSymbol,
      impFlag: Boolean,  /** true if this arg is has 'implicit' declaration */
      overFlag: Boolean, /**                     has 'override' declaration */
      valFlag: Boolean,  /**                     has 'val'      declaration */
      name: String,
      tpe: STpeExpr,
      default: Option[SExpr] = None,
      annotations: List[SArgAnnotation] = Nil,
      isTypeDesc: Boolean = false
  ) extends SMethodOrClassArg with SEntityItem {
    val symbol = SEntityItemSymbol(owner, name, DefType.ClassArg)
    override def tpeArgs: STpeArgs = Nil
    override def isAbstract: Boolean = false
    override def isImplicit: Boolean = impFlag
    override def argss: List[List[SMethodOrClassArg]] = Nil
    override def rhs: Option[SExpr] = None
    override def tpeRes: Option[STpeExpr] = Some(tpe)
    override def exprType = tpeRes
  }

  trait SMethodOrClassArgs {
    def args: List[SMethodOrClassArg]
  }

  case class SMethodArgs(args: List[SMethodArg]) extends SMethodOrClassArgs

  case class SClassArgs(args: List[SClassArg]) extends SMethodOrClassArgs

  case class SSelfTypeDef(name: String, components: List[STpeExpr]) {
    def tpe = components.mkString(" with ")
  }

  type Module = SUnitDef

  /** Correspond to TmplDef syntax construct of Scala.
    * (See http://scala-lang.org/files/archive/spec/2.12/05-classes-and-objects.html)
    */
  abstract class SEntityDef extends SBodyItem with NamedDef { thisEntity =>
    def owner: SSymbol   // actually SUnitSymbol | SEntitySymbol
    def name: String
    val symbol: SEntitySymbol = SEntityDefSymbol(owner, name)

    def tpeArgs: List[STpeArg]

    def ancestors: List[STypeApply]

    def body: List[SBodyItem]

    def selfType: Option[SSelfTypeDef]

    def companion: Option[SEntityDef]

    def isTrait: Boolean

    def isWrapper(implicit ctx: AstContextBase): Boolean = name match {
      case ctx.WrapperEntity(_,_,_) => true
      case _ => false
    }

    def isLiftable(implicit ctx: AstContextBase): Boolean = {
      getAnnotation(LiftableAnnotation) match {
        case Some(SEntityAnnotation(_,_,_)) => true
        case _ => false
      }
    }

    def isConvertible(implicit ctx: AstContextBase): Boolean = {
      getAnnotation(ConvertibleAnnotation) match {
        case Some(SEntityAnnotation(_,_,_)) => true
        case _ => false
      }
    }

    def asTrait: STraitDef = { assert(this.isInstanceOf[STraitDef], s"$this is not trait"); this.asInstanceOf[STraitDef] }
    def asClass: SClassDef = { assert(this.isInstanceOf[SClassDef], s"$this is not class"); this.asInstanceOf[SClassDef] }
    def asObject: SObjectDef = { assert(this.isInstanceOf[SObjectDef], s"$this is not object"); this.asInstanceOf[SObjectDef] }
    def asUnit: SUnitDef = { assert(this.isInstanceOf[SUnitDef], s"$this is not unit"); this.asInstanceOf[SUnitDef] }

    def annotations: List[SEntityAnnotation]

    def args: SClassArgs

    def implicitArgs: SClassArgs

    //====================================================================
    // The following methods allow to collect methods recursively
    // over inheritance hierarchy using given predicate
    def collectItemsInBody(p: SEntityItem => Boolean): List[SEntityMember] = {
      val impValArgs = implicitArgs.args.filter(a => p(a) && a.valFlag).map(SEntityMember(this, _: SEntityItem))
      val valArgs = args.args.filter(a => p(a) && a.valFlag).map(SEntityMember(this, _: SEntityItem))
      val bodyItems = body.collect {
        case ei: SEntityItem if p(ei) => SEntityMember(this, ei)
      }
      valArgs ++ impValArgs ++ bodyItems
    }

    /** Direct inheritance relation which forms DAG */
    def collectAncestorEntities(implicit context: AstContextBase): List[(SEntityDef, List[STpeExpr])] = {
      ancestors.collect { case STypeApply(STraitCall(context.Entity(m, e), args), _) => (e, args) }
    }

    def collectMethodsFromAncestors(p: SEntityItem => Boolean)(implicit context: AstContextBase): Seq[SEntityMember] = {
      collectAncestorEntities.flatMap(_._1.collectAvailableMethods(p))
    }

    def collectAvailableMethods(p: SEntityItem => Boolean)(implicit context: AstContextBase): Seq[SEntityMember] = {
      collectItemsInBody(p) ++ collectMethodsFromAncestors(p)
    }
    //--------------------------------------------------------------------

    /** See http://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#class-linearization*/
    def linearization(implicit context: AstContextBase): List[SEntityDef] = {
      val merged = mutable.LinkedHashMap.empty[String, SEntityDef]
      for (anc <- collectAncestorEntities) {
        val ancLins = anc._1.linearization
        for (ancL <- ancLins) {
          if (!merged.contains(ancL.name)) {
            merged += (ancL.name -> ancL)
          }
        }
      }
      this :: merged.values.toList
    }

    /** Returns substitution for each type arg of each ancestor (e, a1) -> t1 ... (e, aN) -> tN
      * See example to understand the code:
      * trait <e>[a1..aN] { }
      * trait|class <this> extends <e>[t1,...,tN], ...
      */
    def collectAncestorEntitiesWithSubst(implicit ctx: AstContextBase): List[(SEntityDef, List[(STpeArg, STpeExpr)])] = {
      val res = collectAncestorEntities.map { case (e, args) => (e, e.tpeArgs zip args) }
      res
    }

    def linearizationWithSubst
        (subst: STpeSubst)(implicit context: AstContextBase): List[(SEntityDef, List[STpeExpr])] = {
      val merged = mutable.LinkedHashMap.empty[String, (SEntityDef, List[STpeExpr])]
      for ( (anc, args) <- collectAncestorEntitiesWithSubst ) {
        val ancSubst = args.map { case (a, t) => (a.name, t.applySubst(subst)) }.toMap
        val ancLins = anc.linearizationWithSubst(ancSubst)
        for ( (ancL, ancLArgs) <- ancLins ) {
          if (!merged.contains(ancL.name)) {
            merged.put(ancL.name, (ancL, ancLArgs))
          }
        }
      }
      (this, tpeArgs.map(_.toTraitCall.applySubst(subst))) :: merged.values.toList
    }

    def collectVisibleMembers(implicit context: AstContextBase): List[SEntityMember] = {
      val lin = linearizationWithSubst(Map())
      val members = mutable.LinkedHashMap.empty[String, SEntityMember]
      def substTpeArgs(item: SEntityItem, argSubst: STpeSubst): SEntityItem =
        if (argSubst.isEmpty) item
        else {
          item match {
            case md: SMethodDef =>
              val newArgs = md.tpeArgs.map { a =>
                argSubst.get(a.name) match {
                  case Some(t) => a.copy(name = t.name)
                  case None => a
                }
              }
              md.copy(tpeArgs = newArgs)
            case _ => item
          }
        }
      def addMember(e: SEntityDef, args: List[STpeExpr], m: SEntityMember) = {
        val entSubst = e.createTpeSubst(args)
        val itemSubst = disambiguateNames(m.item.tpeArgs.map(_.name), entSubst)
        val item = substTpeArgs(m.item, itemSubst)
        members += (item.name -> SEntityMember(e, item.applySubst(entSubst ++ itemSubst)))
      }
      for ((e, args) <- lin) {
        val ms = e.collectItemsInBody(_ => true)
        for (member <- ms) {
          val name = member.item.name
          if (members.contains(name)) {
            val mExisting = members(name)
            if (!mExisting.matches(member)) {
              addMember(e, args, member)
            }
          } else {
            addMember(e, args, member)
          }
        }
      }
      members.values.toList
    }

    def setOfAvailableNoArgMethods(implicit context: AstContextBase): Set[String] = {
      collectAvailableMethods(_.hasNoArgs).map(_.item.name).toSet
    }

    def findMethodInBody(name: String): Option[SMethodDef] = {
      body.collectFirst { case m: SMethodDef if m.name == name => m }
    }

    def findValInBody(name: String): Option[SValDef] = {
      body.collectFirst { case v: SValDef if v.name == name => v }
    }

    def firstAncestorType = ancestors.headOption.map(_.tpe)

    def hasHighKindTpeArg = tpeArgs.exists(_.isHighKind)

    def setOfAbstractNoArgMethodsInAncestors(implicit context: AstContextBase): Set[String] = {
      collectMethodsFromAncestors(m => m.hasNoArgs && m.isAbstract).map(_.item.name).toSet
    }

    def setOfConcreteNoArgMethodsInAncestors(implicit context: AstContextBase): Set[String] = {
      collectMethodsFromAncestors(m => m.hasNoArgs && !m.isAbstract).map(_.item.name).toSet
    }

    def isAbstractInAncestors(propName: String)(implicit context: AstContextBase) = {
      setOfAbstractNoArgMethodsInAncestors.contains(propName)
    }

    def isConcreteInAncestors(propName: String)(implicit context: AstContextBase) = {
      setOfConcreteNoArgMethodsInAncestors.contains(propName)
    }

    def getMethodsWithAnnotation(annClass: String) = body.collect {
      case md: SMethodDef if md.annotations.exists(a => a.annotationClass == annClass) => md
    }

    def getAncestorTypeNames(implicit context: AstContextBase): List[String] = {
      ancestors.collect { case STypeApply(STraitCall(name,_), _) => name }
    }

    /** Assume no cyclic inheritance. It should be prevented by type checking. */
    def getInheritedTypes(implicit ctx: AstContextBase): List[STraitCall] = {
      val ancs = ancestors.collect { case STypeApply(tc, _) => tc }
      val res = ancs.flatMap {
        case tc @ STraitCall(ctx.Entity(_, e), _) => tc :: e.getInheritedTypes
        case t => List(t)
      }
      res.distinct
    }

    def inherits(traitName: String)(implicit ctx: AstContextBase): Boolean = {
      getInheritedTypes.exists(_.name == traitName)
    }

    def getImplicitArgsForTpeArgs: SClassArgs = {
      val args: List[SClassArg] = tpeArgs.map { a =>
        val (argName, tpe) = if (a.isHighKind)
          ("c" + a.name, STraitCall("Cont", List(STraitCall(a.name))))
        else
          ("e" + a.name, STraitCall("Elem", List(STraitCall(a.name))))
        SClassArg(owner, true, false, true, argName, tpe, None, Nil, true)
      }
      SClassArgs(args)
    }

    def getDeclaredElems(implicit context: AstContextBase): List[(String, STpeExpr)] = {
      val res = (this :: collectAncestorEntities.map(_._1))
        .flatMap(e => {
          val elems = e.body.collect {
            case SMethodDef(_, name, _, _, Some(elemOrCont), true, _, _, _, _, true) =>
              (name, elemOrCont)
          }
          elems
        })
      res
    }

    def getAnnotation(annotName: String) = annotations.find(a => a.annotationClass == annotName)

    def hasAnnotation(annotName: String) = getAnnotation(annotName).isDefined

    def clean: SEntityDef
  }

  case class STraitDef(
      owner: SSymbol,
      name: String,
      tpeArgs: List[STpeArg],
      ancestors: List[STypeApply],
      body: List[SBodyItem],
      selfType: Option[SSelfTypeDef],
      companion: Option[SEntityDef],
      annotations: List[SEntityAnnotation] = Nil) extends SEntityDef with Updatable[STraitDef] {
    def isTrait = true
    def signature = DefSig(DefType.Entity, name, Nil)
    val args = SClassArgs(Nil)
    val implicitArgs: SClassArgs = SClassArgs(Nil)

    def clean = {
      val _companion = companion.map(_.clean)
      copy(
        body = Nil,
        companion = _companion
      )
    }
  }

  implicit class SEntityDefOps(e: SEntityDef) {
    def getExternalName: Option[String] = e.getAnnotation(ExternalAnnotation) match {
      case Some(ExternalEntityAnnotation(externalName)) => Some(externalName)
      case _ => None
    }
    def findMemberInBody(name: String) = e.collectItemsInBody(_.name == name).headOption
    def createTpeSubst(args: List[STpeExpr]): STpeSubst = e.tpeArgs.map(_.name).zip(args).toMap
  }

  implicit class NamedDefTraversableOps[T <: NamedDef, Source[X] <: GenIterable[X]](xs: Source[T]) {
    def apply(name: String): T = xs.find(_.name == name).get
  }

  case class SClassDef(
      owner: SSymbol,
      name: String,
      tpeArgs: List[STpeArg],
      args: SClassArgs,
      implicitArgs: SClassArgs,
      ancestors: List[STypeApply],
      body: List[SBodyItem],
      selfType: Option[SSelfTypeDef],
      companion: Option[SEntityDef],
      isAbstract: Boolean,
      annotations: List[SEntityAnnotation] = Nil) extends SEntityDef with Updatable[SClassDef] {
    def isTrait = false
    def signature = DefSig(DefType.Entity, name, Nil)
    def clean = {
      val _companion = companion.map(_.clean)
      copy(
        body = Nil,
        companion = _companion
      )
    }
  }

  case class SObjectDef(
      owner: SSymbol,
      name: String,
      ancestors: List[STypeApply],
      body: List[SBodyItem]) extends SEntityDef with Updatable[SObjectDef] {
    val args = SClassArgs(Nil)
    def signature = DefSig(DefType.Object, name, Nil)
    def tpeArgs = Nil

    def selfType = None

    def companion = None

    def isTrait = false

    def annotations = Nil

    def implicitArgs = SClassArgs(Nil)

    def clean = {
      copy(
        body = Nil
      )
    }
  }

  case class SDeclaredImplementation(explicitMethods: List[SMethodDef]) {
    def containsMethodDef(m: SMethodDef) =
      explicitMethods.exists { em =>
        em.name == m.name && em.allArgs == m.allArgs &&
          em.tpeArgs == m.tpeArgs
      }
  }

  case class SDeclaredImplementations(declarations: Map[String, SDeclaredImplementation]) {
    def containsMethodDef(name: String, m: SMethodDef) =
      declarations.get(name) match {
        case Some(decl) => decl.containsMethodDef(m)
        case None => false
      }
  }

  type Entity = SEntityDef

  /** Gets module name by its entity. TODO: Should be a general solution. */
  def mod(name: String) = name + "s"
  /** Converts the name of external type to the name of its wrapper. */
  def wrap(name: String) = "W" + name
  /** Converts the name of external type to the name of the module which
    * contains a wrapper for the type. */
  def wmod(name: String) = "W" + mod(name)
  /** Gets name of companion by entity name */
  def comp(name: String) = name + "Companion"
  /** Gets name of the target package to put wrapper based on original package name */
  def wrapPackage(packageName: String) = "wrappers." + packageName

  /** Classification of external types by their names. */
  def isPrimitive(name: String): Boolean = {
    STpePrimitives.contains(name)
  }
  def isStandardType(name: String): Boolean = {
    Set("Tuple", "Function").exists(name.startsWith(_)) ||
      Set("ClassTag").contains(name)
  }

  trait NamedDef {
    def name: String
    def owner: SSymbol
    def symbol: SNamedDefSymbol
    def getModuleTraitName: String = SUnitDef.moduleTraitName(name)
  }

  case class SUnitDef(packageName: String,
      imports: List[SImportStat],
      name: String,
      typeDefs: List[STpeDef],
      traits: List[STraitDef],
      classes: List[SClassDef],
      methods: List[SMethodDef],
      selfType: Option[SSelfTypeDef],
      ancestors: List[STypeApply],
      origModuleTrait: Option[STraitDef], // original module trait declared
      isVirtualized: Boolean,
      okEmitOrigModuleTrait: Boolean = true)
      (@transient implicit val context: AstContextBase)
    extends SEntityDef with Updatable[SUnitDef] {
    def owner: SSymbol = SNoSymbol
    def signature = DefSig(DefType.Unit, name, Nil)
    val unitSym = context.newUnitSymbol(packageName, name)
    override val symbol: SUnitDefSymbol = unitSym
    def unitName = unitSym.unitName
    def getUnitKey: String = unitName.mkFullName

    def tpeArgs: List[STpeArg] = Nil
    def body: List[SBodyItem] = typeDefs ++ traits ++ classes ++ methods
    def companion = None
    def isTrait: Boolean = false
    def annotations: List[SEntityAnnotation] = Nil
    val args = SClassArgs(Nil)
    val implicitArgs = SClassArgs(Nil)

    def getEntity(name: String): SEntityDef = {
      findEntity(name).getOrElse {
        sys.error(s"Cannot find entity with name $name: available entities ${traits.map(_.name)}")
      }
    }
    def findEntity(name: String): Option[Entity] = {
      traits.collectFirst { case e if e.name == name => e }
          .orElse(classes.collectFirst { case c if c.name == name => c })
    }

    def isTrait(name: String) = traits.exists(e => e.name == name)

    def isClass(name: String) = classes.exists(c => c.name == name)

    def allEntities: List[SEntityDef] = traits ++ classes

    /** Collects all WrapSpec entities from this unit. */
    def collectWrapSpecs: List[SEntityDef] = {
      ???
    }

    def dependencies: Seq[SUnitDef] = {
      Seq() // TODO collect dependencies for the module
    }

    def clean = {
      val _entities = traits.map(_.clean)
      val _concreteSClasses = classes.map(_.clean)
      copy(
        imports = Nil,
        typeDefs = Nil,
        traits = _entities,
        classes = _concreteSClasses,
        methods = Nil,
        origModuleTrait = None,
        ancestors = Nil
      )
    }

    def printAst(ast: SUnitDef): Unit = {
      val entityNames = ast.traits.map(_.name).mkString(",")
      val concreteClassNames = ast.classes.map(_.name).mkString(",")
      print(
        s"""
          | Package name: ${ast.packageName}
          | Module name: ${ast.name}
          | Entity: $entityNames
          | Concrete Classes: $concreteClassNames
      """)
    }
  }

  object SUnitDef {
    /** Module trait name related to main trait */
    def moduleTraitName(traitName: String) = traitName + "Module"
    def tpeUseExpr(arg: STpeArg): STpeExpr = STraitCall(arg.name, arg.tparams.map(tpeUseExpr(_)))
  }

  /** Helper class to represent an entity in a module.
    * STmplDef cannot have direct reference to module due to immutability of ScalanAst.
    * This class implements equality and can be used as a key in a Map and as element of Set. */
  class ModuleEntity(val module: SUnitDef, val entity: SEntityDef) {
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      case other: ModuleEntity =>
         module.packageName == other.module.packageName  &&
         module.name == other.module.name &&
         entity.name == other.entity.name
      case _ => false
    })
    override def hashCode = Objects.hash(module.packageName, module.name, entity.name)
  }
  object ModuleEntity {
    def apply(m: Module, e: SEntityDef) = new ModuleEntity(m, e)
    def unapply(me: ModuleEntity) = Some((me.module, me.entity))
  }

  object TypeDescTpe {
    val DescNames = Set("Elem", "Cont", "Functor")
    def unapply(tpe: STpeExpr): Option[(String, STpeExpr)] = tpe match {
      case STraitCall(tname, List(STpeAnnotated(arg,_))) if DescNames.contains(tname) => Some((tname, arg))
      case STraitCall(tname, List(arg)) if DescNames.contains(tname) => Some((tname, arg))
      case _ => None
    }
  }

  class PredefTpe1(name: String) {
    def apply(tpe: STpeExpr) = STraitCall(name, List(tpe))
    def unapply(tpe: STpeExpr): Option[STpeExpr] = tpe match {
      case STraitCall(`name`, List(arg)) => Some(arg)
      case _ => None
    }
  }

  val ClassTagTpe = new PredefTpe1("ClassTag")
  val RTypeTpe = new PredefTpe1("RType")
  val ElemTpe = new PredefTpe1("Elem")
  val ThunkTpe = new PredefTpe1("Thunk")

  object SourceDescriptorTpe {
    def unapply(tpe: STpeExpr): Option[STpeExpr] = tpe match {
      case ClassTagTpe(arg) => Some(arg)
      case RTypeTpe(arg) => Some(arg)
      case _ => None
    }
  }

  object TypeDescArg {
    def unapply(arg: SMethodOrClassArg): Option[(String, String)] = arg.tpe match {
      case TypeDescTpe(descName, STraitCall(typeName, Nil)) => Some((descName, typeName))
      case _ => None
    }
  }

  object RepeatedArgType {
    def unapply(tpe: STpeExpr): Option[STpeExpr] = tpe match {
      case STraitCall("RepeatedArg", List(targ)) => Some(targ)
      case _ => None
    }
  }

  object RepeatedArg {
    def unapply(arg: SMethodOrClassArg): Option[STpeExpr] = arg.tpe match {
      case RepeatedArgType(tpe) => Some(tpe)
      case _ => None
    }
  }

  case class WrapperConf(baseDir: String, packageName: String, name: String, annotations: List[String] = Nil, imports: List[String] = Nil) extends Conf

  case class NonWrapper(name: String)

  case class WrapperDescr(
    unit: SUnitDef, config: WrapperConf,
    /** True if this wrapper is imported via dependencies. */
    isImported: Boolean = false
  )

  case class KernelType(name: String, confKey: String)

  object KernelType {
    def apply(name: String): KernelType = {
      if (ConfigUtil.joinPath(name) == name)
        KernelType(name, name.toLowerCase)
      else
        throw new IllegalArgumentException(s"${name.toLowerCase} is not a legal unquoted configuration key, supply one explicitly")
    }

    val Scala = KernelType("Scala")
    val Cpp   = KernelType("C++", "cpp")
    val Lua   = KernelType("Lua")
  }

  def optimizeMethodImplicits(m: SMethodDef)(implicit ctx: AstContextBase): SMethodDef = {
    val explicitArgs = m.explicitArgs
    val newSections = m.argSections.filterMap(as => {
      val newArgs = as.args.filter {
        case arg@TypeDescArg(_, tyName) if arg.impFlag =>
          !canBeExtracted(explicitArgs, tyName) || m.hasAnnotation(ReifiedTypeArgAnnotation)
        case _ => true
      }
      if (newArgs.nonEmpty) Some(SMethodArgs(newArgs)) else None
    })
    m.copy(argSections = newSections)
  }

  def optimizeTraitImplicits(t: STraitDef)(implicit context: AstContextBase): STraitDef = {
    val newBody = t.body.map {
      case m: SMethodDef => optimizeMethodImplicits(m)
      case item => item
    }
    val newCompanion = t.companion.map(optimizeComponentImplicits(_))
    t.copy(
      body = newBody,
      companion = newCompanion
    )
  }

  def optimizeObjectImplicits(objDef: SObjectDef): SObjectDef = objDef

  def optimizeComponentImplicits(t: SEntityDef)(implicit context: AstContextBase): SEntityDef = t match {
    case c: SClassDef => optimizeClassImplicits(c)
    case o: SObjectDef => optimizeObjectImplicits(o)
    case t: STraitDef => optimizeTraitImplicits(t)
  }

  def canBeExtracted(args: List[SMethodOrClassArg], tyName: String)(implicit context: AstContextBase) = {
    val res = args.exists(a => STpePath.find(a.tpe, tyName).isDefined)
    res
  }

  def optimizeClassImplicits(c: SClassDef)(implicit ctx: AstContextBase): SClassDef = {
    val optArgs =
      if (c.args.args.isEmpty) c
      else {
        val newArgs = c.implicitArgs.args.filter { _ match {
          case TypeDescArg(_,tyName) =>
            val explicitArgs = c.args.args
            !canBeExtracted(explicitArgs, tyName)
          case _ => true
        }}
        c.copy(implicitArgs = SClassArgs(newArgs))
      }
    val newBody = optArgs.body.map {
      case m: SMethodDef => optimizeMethodImplicits(m)
      case item => item
    }
    optArgs.copy(body = newBody)
  }

  def optimizeUnitImplicits(module: SUnitDef): SUnitDef = {
    implicit val ctx = module.context
    val newTraits = module.traits.map(e => optimizeTraitImplicits(e))
    val newClasses = module.classes.map(c => optimizeClassImplicits(c))
    module.copy(
      traits = newTraits,
      classes = newClasses
    )
  }

  def !!!(msg: String) = {
    throw new IllegalStateException(msg)
  }
}
