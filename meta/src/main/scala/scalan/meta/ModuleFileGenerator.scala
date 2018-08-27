package scalan.meta

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scalan.Entity
import scalan.meta.Base.!!!
import scalan.meta.PrintExtensions._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.util.StringUtil
import scalan.util.StringUtil.StringUtilExtensions
import scalan.util.CollectionUtil.TraversableOps

class ModuleFileGenerator(val codegen: MetaCodegen, unit: SUnitDef, config: UnitConfig) {
  import codegen._
  implicit val context = unit.context

  def getCompanionMethods(e: EntityTemplateData) = e.entity.companion.map { comp =>
    val externalConstrs = comp.getMethodsWithAnnotation(ConstructorAnnotation)
    val externalMethods = comp.getMethodsWithAnnotation(ExternalAnnotation)
    (externalConstrs, externalMethods)
  }

  def externalMethod(method: SMethodDef) = {
    val md = optimizeMethodImplicits(method)
    val msgExplicitRetType = "External methods should be declared with explicit type of returning value (result type)"
    lazy val msgRepRetType = s"Invalid method $md. External methods should have return type of type Rep[T] for some T."
    val allArgs = md.allArgs
    val returnType = md.tpeRes.getOrElse(!!!(msgExplicitRetType))
    val unreppedReturnType = returnType.unRep(unit, config.isVirtualized).getOrElse(!!!(msgRepRetType))
    val elemDecls = extractImplicitElems(unit, method.allArgs, method.tpeArgs, Map())
      .filterMap {
        case (ta, Some(expr)) => Some((ta, expr))
        case _ => None
      }
    val argClassesStr = allArgs.rep(_ => s", classOf[Sym]", "")
    val elemClassesStr = (for {
      a <- md.tpeArgs
      cb <- a.contextBound
    } yield s", classOf[$cb[${a.name}]]").mkString
    val finalArgClasses = argClassesStr + elemClassesStr
    val elemArgs = for {
      a <- md.tpeArgs
      cb <- a.contextBound
    } yield s"${if (cb == "Elem") "element" else "weakTypeTag"}[${a.name}]"
    val finalArgs =
      join(allArgs.rep(a => s"${a.name}"), elemArgs)
    s"""
      |    ${md.declaration(config, md.body.isDefined)} = {
      |      ${elemDecls.rep({ case (ta, expr) => s"implicit val e${ta.name} = $expr" }, "\n")}
      |      mkMethodCall(self,
      |        this.getClass.getMethod("${md.name}"$finalArgClasses),
      |        List($finalArgs),
      |        true, element[$unreppedReturnType]).asRep[$unreppedReturnType]
      |    }
      |""".stripMargin
  }

  def externalConstructor(e: EntityTemplateData, method: SMethodDef) = {
    def genConstr(method: SMethodDef) = {
      val md = optimizeMethodImplicits(method)
      val allArgs = md.allArgs
      s"""
        |    ${md.declaration(config, false)} =
        |      newObjEx[${e.typeUse}](${allArgs.rep(_.name)})
        |""".stripMargin
    }

    genConstr(method.copy(argSections = method.cleanedArgs))
  }

  def entityProxy(e: EntityTemplateData) = {
    val entityName = e.name
    val typesDecl = e.tpeArgsDecl
    s"""
      |  // entityProxy: single proxy for each type family
      |  implicit def proxy$entityName${typesDecl}(p: Rep[${e.typeUse}]): ${e.typeUse} = {
      |    proxyOps[${e.typeUse}](p)(scala.reflect.classTag[${e.typeUse}])
      |  }
      |""".stripAndTrim
  }

  def getSClassExp(clazz: SClassDef) = {
    val c = ConcreteClassTemplateData(unit, clazz)
    s"""
      |${methodExtractorsString(c.unit, config, clazz)}
      |
      |""".stripAndTrim
  }

  def familyView(e: EntityTemplateData) = {
    s"""
      |  case class View${e.name}[A, B](source: Rep[${e.name}[A]], override val innerIso: Iso[A, B])
      |    extends View1[A, B, ${e.name}](${StringUtil.lowerCaseFirst(e.name)}Iso(innerIso)) {
      |    override def toString = s"View${e.name}[$${innerIso.eTo.name}]($$source)"
      |    override def equals(other: Any) = other match {
      |      case v: View${e.name}[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      |      case _ => false
      |    }
      |  }
      |""".stripAndTrim
  }

  def emitContRules(e: EntityTemplateData) = {
    s"""
      |    case view1@View${e.name}(Def(view2@View${e.name}(arr, innerIso2)), innerIso1) =>
      |      val compIso = composeIso(innerIso1, innerIso2)
      |      implicit val eAB = compIso.eTo
      |      View${e.name}(arr, compIso)
      |
         |    case ${e.name}Methods.map(xs, f) => (xs, f) match {
      |      case (_, Def(IdentityLambda())) =>
      |        xs
      |      case (xs: ${e.entityRepSynonym.name}[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
      |        val f1 = f.asRep[a => c]
      |        implicit val eB = iso.eFrom
      |        val s = xs.map(f1 >> iso.fromFun)
      |        val res = View${e.name}(s, iso)
      |        res
      |      case (HasViews(source, Def(contIso: ${e.name}Iso[a, b])), f: RFunc[_, c]@unchecked) =>
      |        val f1 = f.asRep[b => c]
      |        val iso = contIso.innerIso
      |        implicit val eC = f1.elem.eRange
      |        source.asRep[${e.name}[a]].map(iso.toFun >> f1)
      |      case _ =>
      |        super.rewriteDef(d)
      |    }""".stripMargin
  }

  def emitContainerRewriteDef(e: EntityTemplateData) = {
    s"""
      |  object UserType${e.name} {
      |    def unapply(s: Sym): Option[Iso[_, _]] = {
      |      s.elem match {
      |        case e: ${e.name}Elem[a,to] => e.eItem match {
      |          case UnpackableElem(iso) => Some(iso)
      |          case _ => None
      |        }
      |        case _ => None
      |      }
      |    }
      |  }
      |
         |  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
      |    case Def(view: View${e.name}[_, _]) =>
      |      Some((view.source, view.iso))
      |    case UserType${e.name}(iso: Iso[a, b]) =>
      |      val newIso = ${StringUtil.lowerCaseFirst(e.name)}Iso(iso)
      |      val repr = reifyObject(UnpackView(s.asRep[${e.name}[b]], newIso))
      |      Some((repr, newIso))
      |    case _ =>
      |      super.unapplyViews(s)
      |  }).asInstanceOf[Option[Unpacked[T]]]
      |
      |  ${e.entityRepSynonym.declaration}
      |
      |  override def rewriteDef[T](d: Def[T]) = d match {
      |    ${e.when(_.isCont, emitContRules)}
      |    case _ => super.rewriteDef(d)
      |  }
      """.stripMargin
  }

  def emitFileHeader = {
    s"""
      |package ${unit.packageName}
      |
      |${(unit.imports.filterNot(_.inCake) ++ config.extraImports.map(SImportStat(_))).distinct.rep(i => s"import ${i.name}", "\n")}
      |
      |""".stripAndTrim
  }

  def implicitTagsFromElems(t: TemplateData) = t.implicitArgs.flatMap(arg =>
    arg.tpe match {
      case STraitCall(name, List(tpe)) if name == "Elem" =>
        Some(s"      implicit val tag${tpe.toIdentifier } = ${arg.name }.tag")
      case _ => None
    }).mkString("\n")

  def familyCont(e: EntityTemplateData) = {
    def container(name: String, isFunctor: Boolean) = {
      val contType = if (isFunctor) "Functor" else "Cont"
      s"""\n
        |  implicit lazy val container$name: $contType[$name] = new $contType[$name] {
        |    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[$name[A]]
        |    def lift[A](implicit evA: Elem[A]) = element[$name[A]]
        |    def unlift[A](implicit eFT: Elem[$name[A]]) =
        |      cast${e.name}Element(eFT).${e.implicitArgs(0).name}
        |    def getElem[A](fa: Rep[$name[A]]) = fa.elem
        |    def unapply[T](e: Elem[_]) = e match {
        |      case e: ${e.name}Elem[_,_] => Some(e.asElem[${e.name}[T]])
        |      case _ => None
        |    }
        |    ${isFunctor.opt(s"def map[A,B](xs: Rep[$name[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}")}
        |  }
           """.stripMargin
    }

    val entityElem = e.elemTypeUse()
    s"""
      |  implicit def cast${e.name}Element${e.tpeArgsDecl}(elem: Elem[${e.typeUse}]): $entityElem =
      |    elem.asInstanceOf[$entityElem]
      |
        |  ${container(e.name, e.isFunctor)}
      |
        |  case class ${e.name}Iso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, ${e.name}] {
      |    lazy val selfType = new ConcreteIsoElem[${e.name}[A], ${e.name}[B], ${e.name}Iso[A, B]](eFrom, eTo).
      |      asInstanceOf[Elem[IsoUR[${e.name}[A], ${e.name}[B]]]]
      |    def cC = container[${e.name}]
      |    def from(x: Rep[${e.name}[B]]) = x.map(innerIso.fromFun)
      |    def to(x: Rep[${e.name}[A]]) = x.map(innerIso.toFun)
      |  }
      |
        |  def ${StringUtil.lowerCaseFirst(e.name)}Iso[A, B](innerIso: Iso[A, B]) =
      |    reifyObject(${e.name}Iso[A, B](innerIso)).asInstanceOf[Iso1[A, B, ${e.name}]]
      |""".stripAndTrim
  }

  def familyElem(e: EntityTemplateData) = {
    val wildcardElem = s"${e.name}Elem[${Array.fill(e.tpeArgs.length + 1)("_").mkString(", ")}]"
    val toArgName = {
      // no point converting to set, since it's small and contains is unlikely to be called more than 2 times
      val takenNames = e.name :: e.tpeArgs.map(_.name)
      (Iterator.single("To") ++ Iterator.from(0).map("To" + _)).filterNot(takenNames.contains).next()
    }
    val elemTypeDecl = s"${e.name}Elem[${join(e.tpeArgs.decls, s"$toArgName <: ${e.typeUse}")}]"
    val defaultParentElem =
      if (e.isCont) {
        s"EntityElem1[${e.tpeArgNames.rep()}, $toArgName, ${e.name}](${e.tpeArgNames.rep("_e" + _)}, container[${e.name}])"
      } else {
        s"EntityElem[$toArgName]"
      }

    val (optParent, parentElem) = e.entity.ancestors.collectFirst {
      case STypeApply(parent@STraitCall(context.Entity(m, e), parentTpeArgs), _) =>
        (Some(parent), s"${e.name}Elem[${join(parentTpeArgs, toArgName)}]")
    }.getOrElse(
      e.entity.ancestors.collectFirst {
        case STypeApply(STraitCall("Def", _), _) => (None, defaultParentElem)
      }.getOrElse(
        e.firstAncestorType match {
          case Some(p) => !!!(s"Unsupported parent type $p of the entity ${e.name}")
          case None if e.unit.isVirtualized =>
            !!!(s"Entity ${e.name} must extend Def or another entity")
          case None => (None, defaultParentElem)
        }
      )
    )

    val overrideIfHasParent = optParent.ifDefined("override ")
    val elemMethodName = entityElemMethodName(e.name)
    val elemMethodDefinition = {
      if (!unit.methods.exists(_.name == elemMethodName)) {
        val elemType = e.elemTypeUse()
        s"""
          |  implicit def $elemMethodName${e.tpeArgsDecl}${e.implicitArgsDecl()}: Elem[${e.typeUse}] =
          |    cachedElem[$elemType]${e.implicitArgsOrParens}
          |""".stripMargin
      } else ""
    }

    s"""
      |  // familyElem
      |  class $elemTypeDecl${e.implicitArgsDecl("_")}
      |    extends $parentElem {
      |${e.implicitArgs.rep(a => s"    ${(e.entity.isAbstractInAncestors(a.name)).opt("override ")}def ${a.name} = _${a.name}", "\n")}
      |    ${overrideIfHasParent}lazy val parent: Option[Elem[_]] = ${optParent.opt(p => s"Some(${tpeToElemStr(p, e.tpeArgs)})", "None")}
      |    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs(${e.emitTpeArgToDescPairs})
      |    override lazy val tag = {
      |${implicitTagsFromElems(e)}
      |      weakTypeTag[${e.typeUse}].asInstanceOf[WeakTypeTag[$toArgName]]
      |    }
      |    override def convert(x: Rep[Def[_]]) = {
      |      val conv = fun {x: Rep[${e.typeUse}] => convert${e.name}(x) }
      |      tryConvert(element[${e.typeUse}], this, x, conv)
      |    }
      |
         |    def convert${e.name}(x: Rep[${e.typeUse}]): Rep[$toArgName] = {
      |      x.elem${e.t.hasHighKindTpeArg.opt(".asInstanceOf[Elem[_]]")} match {
      |        case _: $wildcardElem => x.asRep[$toArgName]
      |        case e => !!!(s"Expected $$x to have $wildcardElem, but got $$e", x)
      |      }
      |    }
      |    override def getDefaultRep: Rep[$toArgName] = ???
      |  }
      |$elemMethodDefinition
      |""".stripAndTrim
  }

  def companionAbs(e: EntityTemplateData) = {
    val entityCompOpt = e.entity.companion
    val hasCompanion = e.entity.companion.isDefined
    s"""
      |  implicit case object ${e.companionName}Elem extends CompanionElem[${e.companionAbsName}] {
      |    lazy val tag = weakTypeTag[${e.companionAbsName}]
      |    protected def getDefaultRep = R${e.name}
      |  }
      |
         |  abstract class ${e.companionAbsName} extends CompanionDef[${e.companionAbsName}]${hasCompanion.opt(s" with ${e.companionName}")} {
      |    def selfType = ${e.companionName}Elem
      |    override def toString = "${e.name}"
      |    ${entityCompOpt.opt(_ => "")}
      |  }
      |${
      hasCompanion.opt
      s"""
        |  implicit def proxy${e.companionAbsName}(p: Rep[${e.companionAbsName}]): ${e.companionAbsName} =
        |    proxyOps[${e.companionAbsName}](p)
        |""".stripAndTrim
    }
      |""".stripAndTrim
  }

  def emitEntityDefs(e: EntityTemplateData) = {
    val entityCompOpt = e.entity.companion
    val hasCompanion = entityCompOpt.isDefined
    val companionMethods = getCompanionMethods(e).opt { case (constrs, methods) =>
      constrs.rep(md => externalConstructor(e, md), "\n    ") +
        methods.rep(md => externalMethod(md), "\n    ")
    }
    val entityName = e.name
    val companionExpString =
      s"""
        |  lazy val R$entityName: Rep[${e.companionAbsName}] = new ${e.companionAbsName} {
        |    $companionMethods
        |  }
       """.stripMargin

    s"""
      |object $entityName extends EntityObject("$entityName") {
      |
      |${entityProxy(e)}
      |
      |${if (e.isCont) familyCont(e) else ""}
      |
      |${familyElem(e)}
      |
      |${companionAbs(e)}
      |
      |${companionExpString}
      |
      |${e.when(_.isCont, familyView)}
      |
      |${methodExtractorsString(unit, config, e.entity)}
      |
      |} // of object ${e.name}
      |  registerEntityObject("$entityName", $entityName)
      |${e.when(_.isCont, emitContainerRewriteDef)}
      |""".stripMargin
  }

  def emitClasses = {
    val concreteClasses = for {clazz <- unit.classes} yield {
      val e = EntityTemplateData(unit, clazz.collectAncestorEntities(context).head._1)
      val className = clazz.name
      val c = ConcreteClassTemplateData(unit, clazz)
      import c.{tpeArgsDecl, tpeArgsUse, implicitArgsUse}
      val fields = clazz.args.argNames
      val fieldsWithType = clazz.args.argNamesAndTypes(config)
      val fieldTypes = clazz.args.argUnrepTypes(unit, config.isVirtualized)
      val implicitArgsDecl = c.implicitArgsDecl()
      val parent = clazz.ancestors.head.tpe
      val parentTpeArgsStr = parent.args.rep()
      val elemTypeDecl = c.name + "Elem" + tpeArgsDecl
      val eFrom = {
        val elemMethodName = StringUtil.lowerCaseFirst(className + "DataElem")
        if (unit.methods.exists(_.name == elemMethodName))
          s"()($elemMethodName)"
        else {
          @tailrec
          def implElem(args: List[String])(str0: String): String = {
            val size = args.length
            if (size > 2) {
              val str = str0 + s"pairElement(element[${args(0)}], "
              implElem(args.drop(1))(str)
            }
            else {
              if (size > 1) str0 + s"pairElement(element[${args(0)}], element[${args(1)}])"
              else str0 + s"(element[${args(0)}])"
            }
          }

          val args = fieldTypes.map(_.toString)
          args.length match {
            case n if n >= 2 =>
              val impls = implElem(args)("")
              val sk = ")" * (n - 2)
              impls + sk
            case 1 => s"element[${args(0)}]"
            case 0 => "UnitElement"
          }
        }
      }
      val parentElem = tpeToElemStr(parent, c.tpeArgs)
      val hasCompanion = clazz.companion.isDefined
      val dataTpe = s"${className}Data$tpeArgsUse"
      val concreteElemSuperType = if (e.isCont)
        s"ConcreteElem1[${join(parentTpeArgsStr, dataTpe, c.typeUse, parent.name)}]"
      else
        s"ConcreteElem[$dataTpe, ${c.typeUse}]"

      def converterBody = {
        val entity = context.findModuleEntity(parent.name)
            .getOrElse(!!!(s"Cannot find parent entity ${parent.name} of class ${clazz.name}"))._2
        val entityFields = entity.setOfAvailableNoArgMethods
        val classFields = clazz.args.args.map(_.name)
        val missingFields = classFields.filterNot(entityFields.contains(_))
        if (missingFields.isEmpty) {
          val args = classFields.rep(f => s"x.$f")
          s"R$className($args)"
        }
        else {
          val msg = s"from ${entity.name} to ${clazz.name}: missing fields $missingFields"
          s"""|// Converter is not generated by meta
              |!!!("Cannot convert $msg")""".stripMargin
        }
      }

      // TODO is the eTo override needed?
      // note: ${className}Iso.eTo doesn't call cachedElem because
      // they are already cached via Isos + lazy val and this would lead to stack overflow
      val isoProductArity = c.implicitArgs.length
      val isoProductElementBody = isoProductArity match {
        case 0 => "???"
        case 1 => c.implicitArgs(0).name
        case _ =>
          val cases = c.implicitArgs.zipWithIndex.map { case (arg, i) =>
            s"      case $i => ${arg.name}"
          }.mkString("\n")
          s"n match {\n$cases\n    }"
      }
      val b = c.extractionBuilder()
      val abstractDescriptors = c.c.collectVisibleMembers.filter { m => m.item.isAbstract && m.item.isTypeDesc }
      val nonExtractableDescriptors = abstractDescriptors.map { m =>
        val extracted = b.extractableArgs.collectFirst {
          case (n, (tyArg, s))
              if STraitCall("Elem", List(tyArg.toTraitCall)) == m.item.tpeRes.get ||
                  STraitCall("Cont", List(tyArg.toTraitCall)) == m.item.tpeRes.get =>
            tyArg.classOrMethodArgName(n)
        }
        (m, extracted)
      }
      val elemDefs = nonExtractableDescriptors.rep({
        case (m, None) =>
          val tc @ STraitCall(descName, List(tyStr)) = m.item.tpeRes.get
          s"override lazy val ${m.item.name}: $descName[$tyStr] = implicitly[$descName[$tyStr]]"
        case (m, Some(extractedName)) =>
          val STraitCall(descName, List(tyStr)) = m.item.tpeRes.get
          s"override lazy val ${m.item.name}: $descName[$tyStr] = $extractedName"
      }, "\n|").stripMargin

      s"""
        |object $className extends EntityObject("$className") {
        |  case class ${c.typeDecl("Ctor") }
        |      (${fieldsWithType.rep(f => s"override val $f") })${c.optimizeImplicits().implicitArgsDecl() }
        |    extends ${c.typeUse }(${fields.rep() })${clazz.selfType.opt(t => s" with ${t.tpe }") } with Def[${c.typeUse }] {
        |    ${b.extractableImplicits(inClassBody = true) }
        |    ${elemDefs}
        |    lazy val selfType = element[${c.typeUse }]
        |  }
        |  // elem for concrete class
        |  class $elemTypeDecl(val iso: Iso[$dataTpe, ${c.typeUse}])${c.implicitArgsDeclConcreteElem}
        |    extends ${parent.name}Elem[${join(parentTpeArgsStr, c.typeUse)}]
        |    with $concreteElemSuperType {
        |    override lazy val parent: Option[Elem[_]] = Some($parentElem)
        |    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs(${c.emitTpeArgToDescPairs})
        |    override def convert${parent.name}(x: Rep[$parent]) = $converterBody
        |    override def getDefaultRep = R$className(${fieldTypes.rep(zeroSExpr(e.entity)(_))})
        |    override lazy val tag = {
        |${implicitTagsFromElems(c)}
        |      weakTypeTag[${c.typeUse}]
        |    }
        |  }
        |
        |  // state representation type
        |  type ${className}Data${tpeArgsDecl} = ${dataType(fieldTypes)}
        |
        |  // 3) Iso for concrete class
        |  class ${className}Iso${tpeArgsDecl}${implicitArgsDecl}
        |    extends EntityIso[$dataTpe, ${c.typeUse}] with Def[${className}Iso$tpeArgsUse] {
        |    private lazy val _safeFrom = fun { p: Rep[${c.typeUse }] => ${fields.map(fields => "p." + fields).opt(s => if (s.toList.length > 1) s"(${s.rep() })" else s.rep(), "()") } }
        |    override def from(p: Rep[${c.typeUse}]) =
        |      tryConvert[${c.typeUse}, ${dataType(fieldTypes)}](eTo, eFrom, p, _safeFrom)
        |    override def to(p: Rep[${dataType(fieldTypes)}]) = {
        |      val ${pairify(fields)} = p
        |      R$className(${fields.rep()})
        |    }
        |    lazy val eFrom = $eFrom
        |    lazy val eTo = new ${c.elemTypeUse}(self)
        |    lazy val selfType = new ${className}IsoElem$tpeArgsUse$implicitArgsUse
        |    def productArity = $isoProductArity
        |    def productElement(n: Int) = $isoProductElementBody
        |  }
        |  case class ${className}IsoElem${tpeArgsDecl}(${c.implicitArgs.rep(a => s"${a.name}: ${a.tpe}")}) extends Elem[${className}Iso$tpeArgsUse] {
        |    def getDefaultRep = reifyObject(new ${className}Iso${tpeArgsUse}()$implicitArgsUse)
        |    lazy val tag = {
        |${implicitTagsFromElems(c)}
        |      weakTypeTag[${className}Iso$tpeArgsUse]
        |    }
        |    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs(${c.emitTpeArgToDescPairs})
        |  }
        |  // 4) constructor and deconstructor
        |  class ${c.companionAbsName} extends CompanionDef[${c.companionAbsName}]${hasCompanion.opt(s" with ${c.companionName}")} {
        |    def selfType = ${className}CompanionElem
        |    override def toString = "${className}Companion"
        |${
          (fields.length != 1).opt({
            val s = c.entity.args.args.zipWithIndex.map { case (a, i) => a.name -> s"p._${i + 1}" }.toMap
            val sb = c.extractionBuilder(s)
            s"""
              |    @scalan.OverloadId("fromData")
              |    def apply${tpeArgsDecl}(p: Rep[$dataTpe])${c.optimizeImplicits().implicitArgsDecl()}: Rep[${c.typeUse}] = {
              |      ${sb.extractableImplicits(false)}
              |      iso$className${c.tpeArgNames.opt(ns => s"[${ns.rep()}]")}.to(p)
              |    }
              """.stripAndTrim
          })
        }
        |    @scalan.OverloadId("fromFields")
        |    def apply${tpeArgsDecl}(${fieldsWithType.rep()})${c.optimizeImplicits().implicitArgsDecl()}: Rep[${c.typeUse}] =
        |      mk$className(${fields.rep()})
        |
        |    def unapply${tpeArgsDecl}(p: Rep[$parent]) = unmk$className(p)
        |  }
        |  lazy val ${c.name}Rep: Rep[${c.companionAbsName}] = new ${c.companionAbsName}
        |  lazy val R${c.name}: ${c.companionAbsName} = proxy${className}Companion(${c.name}Rep)
        |  implicit def proxy${className}Companion(p: Rep[${c.companionAbsName}]): ${c.companionAbsName} = {
        |    proxyOps[${c.companionAbsName}](p)
        |  }
        |
        |  implicit case object ${className}CompanionElem extends CompanionElem[${c.companionAbsName}] {
        |    lazy val tag = weakTypeTag[${c.companionAbsName}]
        |    protected def getDefaultRep = ${className}Rep
        |  }
        |
        |  implicit def proxy${c.typeDecl}(p: Rep[${c.typeUse}]): ${c.typeUse} =
        |    proxyOps[${c.typeUse}](p)
        |
        |  implicit class Extended${c.typeDecl}(p: Rep[${c.typeUse}])${c.optimizeImplicits().implicitArgsDecl()} {
        |    def toData: Rep[$dataTpe] = {
        |      ${c.extractionBuilder(prefix = "p.").extractableImplicits(false)}
        |      iso$className${c.implicitArgsUse}.from(p)
        |    }
        |  }
        |
        |  // 5) implicit resolution of Iso
        |  implicit def iso${c.typeDecl}${implicitArgsDecl}: Iso[$dataTpe, ${c.typeUse}] =
        |    reifyObject(new ${className}Iso${tpeArgsUse}()$implicitArgsUse)
        |
        |  def mk${c.typeDecl }
        |    (${fieldsWithType.rep() })${c.optimizeImplicits().implicitArgsDecl() }: Rep[${c.typeUse }] = {
        |    new ${c.typeUse("Ctor") }(${fields.rep() })
        |  }
        |  def unmk${c.typeDecl }(p: Rep[$parent]) = p.elem.asInstanceOf[Elem[_]] match {
        |    case _: ${c.elemTypeUse } @unchecked =>
        |      Some((${fields.rep(f => s"p.asRep[${c.typeUse }].$f") }))
        |    case _ =>
        |      None
        |  }
        |
        |  ${methodExtractorsString(unit, config, clazz) }
        |
        |} // of object $className
        |  registerEntityObject("$className", $className)
        |""".stripAndTrim
    }
    concreteClasses.mkString("\n\n")
  }

  def emitModuleDefs = {
    val entities = for {entity <- unit.traits} yield {
      val e = EntityTemplateData(unit, entity)
      emitEntityDefs(e)
    }
    val imports = {
      val predef = List("import IsoUR._", "import Converter._")
      val declaredImports = unit.imports.filter(_.inCake)
      val used = unit.getUsedEntities
      val ts = unit.traits.map(t => context.newEntitySymbol(unit.symbol, t.name))
      val cs = unit.classes.map(c => context.newEntitySymbol(unit.symbol, c.name))
      val entityObjects = (used ++ ts ++ cs).map(s => s"import ${s.name}._")
      val res = predef ++ declaredImports.distinct.map(i => s"import ${i.name}") ++ entityObjects
      res.distinct
    }

    s"""
      |// Abs -----------------------------------
      |trait ${unit.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${unit.name} {
      |  ${unit.selfTypeString("")}
      |${imports.mkString("\n")}
      |
      |${entities.mkString("\n\n")}
      |
      |${emitClasses}
      |
      |  registerModule(${unit.name}Module)
      |}
      |""".stripAndTrim
  }

  def emitModuleInfo = {
    val moduleName = unit.name
    s"""
      |object ${moduleName}Module extends scalan.ModuleInfo("${unit.packageName}", "$moduleName")
       """.stripMargin
  }

  def emitDslTraits = {
    unit.origModuleTrait match {
      case Some(mt) if unit.okEmitOrigModuleTrait =>
        s"trait ${mt.name} extends ${unit.packageName}.${mt.ancestors.rep(a => a.tpe.toString, " with ")}"
      case None if unit.okEmitOrigModuleTrait =>
        val moduleTraitName = unit.getModuleTraitName
        val selfTypeStr = unit.selfType match {
          case Some(SSelfTypeDef(_, List(STraitCall(`moduleTraitName`, Nil)))) => ""
          case _ => s" {${unit.selfTypeString("")}}"
        }
        s"trait $moduleTraitName extends ${unit.packageName}.impl.${unit.name}Defs$selfTypeStr"
      case _ => ""
    }
  }

  def emitImplFile: String = {
    val topLevel = List(
      emitFileHeader,
      "package impl {",
      emitModuleDefs,
      emitModuleInfo,
      "}",
      emitDslTraits
    )
    topLevel.mkString("", "\n\n", "\n").
      // clean empty lines
      replaceAll(""" +\r?\n""", "\n").
      // remove 3 or more consecutive linebreaks
      replaceAll("""(\r?\n){3,}""", "\n\n").
      // remove empty lines before and after braces
      replaceAll("""\r?\n\r?\n( *\})""", "\n$1").
      replaceAll("""( *\{)\r?\n\r?\n""", "$1\n")
  }
}
