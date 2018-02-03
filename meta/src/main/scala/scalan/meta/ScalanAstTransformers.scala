package scalan.meta

import scala.collection.mutable
import scalan.meta
import scalan.meta.ScalanAst._
import scalan.util.CollectionUtil._

object ScalanAstTransformers {
  /** The class implements a default Meta AST transformation strategy: breadth-first search */
  class AstTransformer(implicit val ctx: AstContext) extends (SExpr => SExpr) {
    override def apply(e: SExpr): SExpr = exprTransform(e)
    def constTransform(c: SConst): SConst = c
    def identTransform(ident: SIdent): SExpr = ident
    def selectTransform(select: SSelect): SExpr = {
      val newExpr = exprTransform(select.expr)
      select.copy(expr = newExpr)
    }
    def applyTransform(apply: SApply): SApply = {
      val newFun = exprTransform(apply.fun)
      val newArgss = apply.argss map (sec => SArgSection(sec.map(exprTransform)))

      apply.copy(fun = newFun, argss = newArgss)
    }
    def tupleTransform(tuple: STuple): STuple = {
      val newExprs = tuple.exprs map exprTransform
      tuple.copy(exprs = newExprs)
    }
    def exprApplyTransform(exprApply: SExprApply): SExprApply = {
      val newFun = exprTransform(exprApply.fun)
      exprApply.copy(fun = newFun)
    }
    def thisTransform(sthis: SThis): SThis = sthis
    def constrTransform(constr: SConstr): SConstr = {
      val newArgs = constr.args mapConserve exprTransform
      constr.copy(args = newArgs)
    }
    def ascrTransform(ascr: SAscr): SAscr = {
      val newExpr = exprTransform(ascr.expr)
      ascr.copy(expr = newExpr)
    }
    def funcTransform(func: SFunc): SFunc = {
      val newParams = func.params mapConserve valdefTransform
      val newRes = exprTransform(func.res)
      func.copy(params = newParams, res = newRes)
    }
    def blockTransform(block: SBlock): SBlock = {
      val newBlockInit = block.init mapConserve exprTransform
      val newBlocklast = exprTransform(block.last)

      block.copy(init = newBlockInit, last = newBlocklast)
    }

    def exprTransform(expr: SExpr): SExpr = expr match {
      case empty: SEmpty => empty
      case c: SConst => constTransform(c)
      case ident: SIdent => identTransform(ident)
      case apply: SApply => applyTransform(apply)
      case exprApply: SExprApply => exprApplyTransform(exprApply)
      case select: SSelect => selectTransform(select)
      case constr: SConstr => constrTransform(constr)
      case sthis: SThis => thisTransform(sthis)
      case ascr: SAscr => ascrTransform(ascr)
      case func: SFunc => funcTransform(func)
      case block: SBlock => blockTransform(block)
      case bodyItem: SBodyItem => bodyItemTransform(bodyItem)
      case tup: STuple => tupleTransform(tup)
      case arg: SClassArg => classArgTransform(arg)
      case m: SMatch => matchTransform(m)
      case c: SCase => caseTransform(c)
      case e: SIf => ifTransform(e)
      case e: SAssign => assignTransform(e)
      case e: SAnnotated => annotatedTransform(e)
      case e: SSuper => superTransform(e)
      case _ => throw new NotImplementedError(s"Cannot exprTransform($expr)")
    }

    def tpeExprTransform(tpe: STpeExpr) = tpe

    def superTransform(e: SSuper) = {
      val newExprType = e.exprType.mapConserve(tpeExprTransform)
      if (e.exprType.eq(newExprType)) e
      else
        e.copy(exprType = newExprType)
    }

    def annotatedTransform(e: SAnnotated) = {
      val newExpr = e.expr.transformConserve(exprTransform)
      val newExprType = e.exprType.mapConserve(tpeExprTransform)
      if (e.expr.eq(newExpr) && e.exprType.eq(newExprType)) e
      else
        e.copy(expr = newExpr, exprType = newExprType)
    }

    def ifTransform(e: SIf) = {
      val newCond = e.cond.transformConserve(exprTransform)
      val newTh = e.th.transformConserve(exprTransform)
      val newEl = e.el.transformConserve(exprTransform)
      val newExprType = e.exprType.mapConserve(tpeExprTransform)
      if (e.cond.eq(newCond) && e.th.eq(newTh) && e.el.eq(newEl) && e.exprType.eq(newExprType)) e
      else
        e.copy( cond = newCond, th = newTh, el = newEl, exprType = newExprType )
    }

    def assignTransform(e: SAssign) = {
      val newLeft = e.left.transformConserve(exprTransform)
      val newRight = e.right.transformConserve(exprTransform)
      val newExprType = e.exprType.mapConserve(tpeExprTransform)
      if (e.left.eq(newLeft) && e.right.eq(newRight) && e.exprType.eq(newExprType)) e
      else
        e.copy( left = newLeft, right = newRight, exprType = newExprType )
    }

    def matchTransform(m: SMatch) = {
      val newSelector = exprTransform(m.selector)
      val newCases = m.cases.mapConserve(caseTransform)
      val newExprType = m.exprType.mapConserve(tpeExprTransform)
      if (m.selector.eq(newSelector) && m.cases.eq(newCases) && m.exprType.eq(newExprType)) m
      else
        m.copy(
          selector = newSelector,
          cases = newCases,
          exprType = newExprType
        )
    }

    def caseTransform(c: SCase) = {
      val newPat = patternTransform(c.pat)
      val newGuard = exprTransform(c.guard)
      val newBody = exprTransform(c.body)
      val newExprType = c.exprType.mapConserve(tpeExprTransform)
      if (c.pat.eq(newPat) && c.guard.eq(newGuard) &&
          c.body.eq(newBody) && c.exprType.eq(newExprType)) c
      else
        c.copy(
          pat = newPat,
          guard = newGuard,
          body = newBody,
          exprType = newExprType
        )
    }

    def patternTransform(p: SPattern) = p match {
      case p: SWildcardPattern => p.transformConserve(wildcardPatternTransform)
      case p: SLiteralPattern  => p.transformConserve(literalPatternTransform)
      case p: SStableIdPattern => p.transformConserve(stableIdPatternTransform)
      case p: SSelPattern   => p.transformConserve(selPatternTransform)
      case p: SAltPattern   => p.transformConserve(altPatternTransform)
      case p: STypedPattern => p.transformConserve(typedPatternTransform)
      case p: SBindPattern  => p.transformConserve(bindPatternTransform)
      case p: SApplyPattern => p.transformConserve(applyPatternTransform)
    }

    def wildcardPatternTransform(arg: SWildcardPattern) = arg
    def literalPatternTransform(arg: SLiteralPattern) = arg
    def stableIdPatternTransform(arg: SStableIdPattern) = arg
    def selPatternTransform(arg: SSelPattern) = arg
    def altPatternTransform(arg: SAltPattern) = arg
    def typedPatternTransform(arg: STypedPattern) = arg
    def bindPatternTransform(arg: SBindPattern) = arg
    def applyPatternTransform(arg: SApplyPattern) = arg

    def methodArgTransform(arg: SMethodArg): SMethodArg = arg
    def methodArgsTransform(args: SMethodArgs): SMethodArgs = {
      val newArgs = args.args mapConserve methodArgTransform

      args.copy(args = newArgs)
    }
    def methodArgSectionsTransform(argSections: List[SMethodArgs]): List[SMethodArgs] = {
      argSections mapConserve methodArgsTransform
    }
    def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res
    def methodBodyTransform(body: Option[SExpr]): Option[SExpr] = body match {
      case Some(bodyExpr) => Some(exprTransform(bodyExpr))
      case None => None
    }
    def methodTransform(method: SMethodDef): SMethodDef = {
      val newArgSections = methodArgSectionsTransform(method.argSections)
      val newTpeRes = methodResTransform(method.tpeRes)
      val newBody = methodBodyTransform(method.body)

      method.copy(
        argSections = newArgSections,
        tpeRes = newTpeRes,
        body = newBody
      )
    }
    def valdefTransform(valdef: SValDef): SValDef = {
      val newExpr = exprTransform(valdef.expr)
      valdef.copy(expr = newExpr)
    }

    def bodyItemTransform(bodyItem: SBodyItem): SBodyItem = bodyItem match {
      case method: SMethodDef => methodTransform(method)
      case valdef: SValDef => valdefTransform(valdef)
      case _ => throw new NotImplementedError(s"${bodyItem}")
    }
    def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body mapConserve bodyItemTransform

    def traitCompTransform(traitComp: STraitDef): STraitDef = {
      val newBody = bodyTransform(traitComp.body)
      traitComp.copy(body = newBody)
    }
    def classCompTransform(classComp: SClassDef): SClassDef = {
      val newBody = bodyTransform(classComp.body)
      classComp.copy(body = newBody)
    }
    def objCompTransform(obj: SObjectDef): SObjectDef = {
      val newBody = bodyTransform(obj.body)
      obj.copy(body = newBody)
    }

    def entityCompTransform(companion: Option[SEntityDef]): Option[SEntityDef] = {
      companion match {
        case Some(tr: STraitDef) => Some(traitCompTransform(tr))
        case Some(clazz: SClassDef) => Some(classCompTransform(clazz))
        case Some(obj: SObjectDef) => Some(objCompTransform(obj))
        case None => None
        case _ => throw new NotImplementedError(s"companion = $companion")
      }
    }

    def entityAncestorTransform(ancestor: STypeApply): STypeApply = ancestor
    def entityAncestorsTransform(ancestors: List[STypeApply]): List[STypeApply] = {
      ancestors mapConserve entityAncestorTransform
    }

    def entityTpeArgTransform(tpeArg: STpeArg): STpeArg = tpeArg
    def entityTpeArgsTransform(tpeArgs: List[STpeArg]): List[STpeArg] = {
      tpeArgs mapConserve entityTpeArgTransform
    }

    def entityTransform(entity: STraitDef): STraitDef = {
      val newTpeArgs = entityTpeArgsTransform(entity.tpeArgs)
      val newBody = bodyTransform(entity.body)
      val newCompanion = entityCompTransform(entity.companion)
      val newAncestors = entityAncestorsTransform(entity.ancestors)

      entity.copy(
        tpeArgs = newTpeArgs,
        body = newBody,
        companion = newCompanion,
        ancestors = newAncestors
      )
    }

    def classCompanionTransform(companion: Option[SEntityDef]): Option[SEntityDef] = {
      companion.map {
        case obj: SObjectDef => obj.copy(body = bodyTransform(obj.body))
        case tr: STraitDef => tr.copy(body = bodyTransform(tr.body))
        case unknown => throw new NotImplementedError(unknown.toString)
      }
    }
    def classArgTransform(classArg: SClassArg): SClassArg = {
      val newDefault = classArg.default.mapConserve(exprTransform)
      classArg.copy(default = newDefault)
    }
    def classArgsTransform(classArgs: SClassArgs): SClassArgs = {
      val newArgs = classArgs.args mapConserve classArgTransform
      classArgs.copy(args = newArgs)
    }
    def classTransform(c: SClassDef): SClassDef = {
      val newBody = bodyTransform(c.body)
      val newCompanion = classCompanionTransform(c.companion)
      val newClassArgs = classArgsTransform(c.args)
      val newImplicitClassArgs = classArgsTransform(c.implicitArgs)
      val newAncestors = entityAncestorsTransform(c.ancestors)
      c.copy(
        args = newClassArgs,
        implicitArgs = newImplicitClassArgs,
        body = newBody,
        companion = newCompanion,
        ancestors = newAncestors
      )
    }

    def moduleTransform(module: SUnitDef): SUnitDef = {
      val newEntities = module.traits mapConserve entityTransform
      val newClasses = module.classes mapConserve classTransform
      module.copy(
        traits = newEntities,
        classes = newClasses
      )
    }
  }

  /** Transform some Meta AST to another AST by applying the repl function to each type name. */
  class AstReplacer(name: String, repl: String => String)(implicit context: AstContext) extends AstTransformer {
    val typeTransformer = new TypeReplacer(name, repl)

    override def methodArgTransform(arg: SMethodArg): SMethodArg = {
      arg.copy(tpe = typeTransformer(arg.tpe))
    }
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res match {
      case Some(traitCall: STraitCall) => Some(typeTransformer.traitCallTransform(traitCall))
      case _ => super.methodResTransform(res)
    }
    override def classArgTransform(classArg: SClassArg): SClassArg = {
      classArg.copy(tpe = typeTransformer(classArg.tpe))
    }
    override def selectTransform(select: SSelect): SExpr = select match {
      case select: SSelect if select.tname == name =>
        SSelect(SEmpty(), repl(select.tname))
      case _ => super.selectTransform(select)
    }
    override def ascrTransform(ascr: SAscr): SAscr = {
      val newPt = typeTransformer(ascr.pt)
      super.ascrTransform(ascr.copy(pt = newPt))
    }
    override def applyTransform(apply: SApply): SApply = {
      val newTs = apply.ts mapConserve typeTransformer.typeTransform
      super.applyTransform(apply.copy(ts = newTs))
    }
    override def exprApplyTransform(exprApply: SExprApply): SExprApply = {
      val newTs = exprApply.ts mapConserve typeTransformer.typeTransform
      super.exprApplyTransform(exprApply.copy(ts = newTs))
    }
    override def valdefTransform(valdef: SValDef): SValDef = {
      val newTpe = valdef.tpe.map(typeTransformer(_))
      super.valdefTransform(valdef.copy(tpe = newTpe))
    }

    override def entityAncestorTransform(ancestor: STypeApply): STypeApply = {
      val newTpe = typeTransformer.traitCallTransform(ancestor.tpe)
      super.entityAncestorTransform(ancestor.copy(tpe = newTpe))
    }
  }

  /** Transforming of Meta AST related to types (children of STpeExpr)*/
  class TypeTransformer extends (STpeExpr => STpeExpr) {
    override def apply(tpe: STpeExpr): STpeExpr = typeTransform(tpe)
    def typeTransform(tpe: STpeExpr): STpeExpr = tpe match {
      case tup: STpeTuple => tupleTransform(tup)
      case func: STpeFunc => funcTransform(func)
      case empty: STpeEmpty => emptyTransform(empty)
      case traitCall: STraitCall => traitCallTransform(traitCall)
      case prim: STpePrimitive => primitiveTransform(prim)
      case existType: STpeExistential => existTypeTransform(existType)
      case _ => tpe
    }

    def tupleTransform(tup: STpeTuple) = STpeTuple(tup.args.mapConserve(typeTransform))
    def funcTransform(func: STpeFunc) = STpeFunc(typeTransform(func.domain), typeTransform(func.range))
    def emptyTransform(emptyType: STpeEmpty) = emptyType
    def primitiveTransform(prim: STpePrimitive) = prim

    def traitCallArgsTransform(args: List[STpeExpr]): List[STpeExpr] = args mapConserve(typeTransform)
    def traitCallNameTransform(name: String): String = name
    def traitCallTransform(traitCall: STraitCall): STraitCall = {
      val newName = traitCallNameTransform(traitCall.name)
      val newArgs = traitCallArgsTransform(traitCall.args)

      traitCall.copy(name = newName, args = newArgs)
    }
    def tpeDefArgTransform(tpeArg: STpeArg): STpeArg = tpeArg
    def tpeDefArgsTransform(tpeArgs: STpeArgs): STpeArgs = {
      tpeArgs mapConserve tpeDefArgTransform
    }
    def tpeDefTransform(tpeDef: STpeDef): STpeDef = {
      val newTpeArgs = tpeDefArgsTransform(tpeDef.tpeArgs)

      tpeDef.copy(tpeArgs = newTpeArgs)
    }
    def existItemTransform(item: SBodyItem): SBodyItem = item match {
      case tpeDef: STpeDef => tpeDefTransform(tpeDef)
      case _ => item
    }
    def existItemsTransform(items: List[SBodyItem]): List[SBodyItem] = {
      items mapConserve existItemTransform
    }
    def existTypeTransform(existType: STpeExistential): STpeExistential = {
      val newTpt = typeTransform(existType.tpt)
      val newItems = existItemsTransform(existType.items)

      existType.copy(tpt = newTpt, items = newItems)
    }
  }

  /** Renaming of all types with the given name by applying the repl function. */
  class TypeReplacer(name: String, repl: String => String) extends TypeTransformer {
    override def traitCallNameTransform(tname: String): String = {
      if (tname == name) repl(tname)
      else tname
    }
  }

  /** Renaming of types with oldName to new name (newName). */
  class TypeRenamer(oldName: String, newName: String) extends TypeTransformer {
    override def traitCallNameTransform(tname: String): String = {
      if (tname == oldName) newName
      else tname
    }
    override def tpeDefArgTransform(tpeArg: STpeArg): STpeArg = {
      if (tpeArg.name == oldName) tpeArg.copy(name = newName)
      else tpeArg
    }
  }

  /** Collect types names from given type term as side effect. */
  class TypeNameCollector(val names: mutable.HashSet[String]) extends TypeTransformer {
    override def traitCallNameTransform(tname: String): String = {
      names += tname
      tname
    }
    override def tpeDefArgTransform(tpeArg: STpeArg): STpeArg = {
      names += tpeArg.name
      tpeArg
    }
  }

  /** Removes all Rep types including RFunc and type synonims. */
  class RepTypeRemover(implicit ctx: AstContext) extends TypeTransformer {
    override def typeTransform(tpe: STpeExpr): STpeExpr = {
      val t = tpe match {
        case ctx.RepTypeOf(t) => t
        case _ => tpe
      }
      super.typeTransform(t)
    }
  }

  /** Transform type by applying given substitution. */
//  class SubstTypeTransformer(subst: STpeSubst) extends TypeTransformer {
//    override def typeTransform(tpe: STpeExpr): STpeExpr = tpe match {
//      case tc: STraitCall if subst.get(tc.name).isDefined =>
//        assert(tc.args.isEmpty,
//          s"""Cannot substitute higher-kind TraitCall $tc:
//            |higher-kind usage of names is not supported  Array[A] - ok, A[Int] - nok""".stripMargin)
//        subst(tc.name)
//      case _ => super.typeTransform(tpe)
//    }
//  }

  class SubstTypeTransformer(subst: STpeSubst) extends TypeTransformer {
    override def typeTransform(tpe: STpeExpr): STpeExpr = tpe match {
      case tc @ STraitCall(n, Nil) if subst.get(n).isDefined =>
        subst(n)
      case tc @ STraitCall(n, args) if subst.get(n).isDefined =>
        val newTpe = subst(n)
        if (newTpe.args.nonEmpty)
          sys.error(s"""Cannot substitute $tc with $newTpe""".stripMargin)
        STraitCall(newTpe.name, args)
      case _ => super.typeTransform(tpe)
    }
  }

  /** Traverse whole META AST and rename types. Returns new tree. */
  class TypeTransformerInAst(typeTrans: TypeTransformer)(implicit ctx: AstContext) extends AstTransformer {
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = {
      res.map(typeTrans)
    }
    override def methodArgTransform(arg: SMethodArg): SMethodArg = {
      arg.copy(tpe = typeTrans(arg.tpe))
    }
    override def classArgTransform(classArg: SClassArg): SClassArg = {
      classArg.copy(tpe = typeTrans(classArg.tpe))
    }

    override def valdefTransform(valdef: SValDef): SValDef = {
      val newVal = valdef.copy(tpe = valdef.tpe.map(typeTrans))
      super.valdefTransform(newVal)
    }
  }

  /** Traverse whole META AST and rename types. Returns new tree. */
  class TypeNameTransformer(oldName: String, newName: String)(implicit ctx: AstContext) extends AstTransformer {
    val typeRenamer = new TypeRenamer(oldName, newName)
    override def entityTpeArgTransform(tpeArg: STpeArg): STpeArg = {
      if (tpeArg.name == oldName) tpeArg.copy(name = newName)
      else tpeArg
    }
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res match {
      case Some(resType) => Some(typeRenamer(resType))
      case _ => res
    }
    override def methodArgTransform(arg: SMethodArg): SMethodArg = {
      val newTpe = typeRenamer(arg.tpe)
      arg.copy(tpe = newTpe)
    }
    override def entityAncestorTransform(ancestor: STypeApply): STypeApply = {
      val newTpe = typeRenamer.traitCallTransform(ancestor.tpe)
      val newTs = ancestor.ts.mapConserve(exprTransform)
      ancestor.copy(tpe = newTpe, ts = newTs)
    }
  }

  /** Removing of internal parts of annotations that should be ignored at code generation. */
  def filterInternalAnnot(annotations: List[SAnnotation]): List[SAnnotation] = {
    annotations map {
      case annotation @ SMethodAnnotation("Constructor", args) =>
        val newArgs = args filter {
          case SAssign(SIdent("original",_), m: SMethodDef,_) => false
          case _ => true
        }
        annotation.copy(args = newArgs)
      case other => other
    }
  }

  /** The external types that should be rejected during virtualization. */
  def isIgnoredExternalType(typeName: String) = Set("Object", "Any", "AnyRef").contains(typeName)

  class EnrichPipeline(implicit val context: AstContext) extends (SUnitDef => SUnitDef) {
    val moduleBuilder = new SModuleBuilder()
    import moduleBuilder._
    private val chain = scala.Function.chain(Seq(
      genClassesImplicits _
    ))
    override def apply(module: Module): Module = chain(module)
  }

  class DevirtPipeline(implicit val context: AstContext) extends (SUnitDef => SUnitDef) {
    val moduleBuilder = new SModuleBuilder()
    import moduleBuilder._
    private val chain = scala.Function.chain(Seq(
      unrepAllTypes _
    ))
    override def apply(module: Module): Module = chain(module)
  }

  class External2WrapperTypeTransformer(name: String)(implicit context: AstContext) extends AstReplacer(name, wrap)

  class ExtType2WrapperTypeTransformer(name: String) extends TypeReplacer(name, wrap)

  class RemoveClassTagFromSignatures(implicit ctx: AstContext) extends AstTransformer {
    override def methodArgsTransform(args: SMethodArgs): SMethodArgs = {
      // filter out ClassTag args
      val newArgs = args.args.filter { marg => marg.tpe match {
        case ClassTagTpe(_) => false
        case _ => true
      }} mapConserve methodArgTransform
      args.copy(args = newArgs)
    }
    override def methodArgSectionsTransform(argSections: List[SMethodArgs]): List[SMethodArgs] = {
      argSections mapConserve methodArgsTransform filterNot { _.args.isEmpty }
    }
    override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = {
      body filter{
        case SMethodDef(_,_,_,_,Some(STraitCall("ClassTag", _)),true,_,_,_,_,_) => false
        case _ => true
      } mapConserve bodyItemTransform
    }
    override def classArgsTransform(classArgs: SClassArgs): SClassArgs = {
      val newArgs = classArgs.args.filter { _.tpe match {
        case ClassTagTpe(_) => false
        case _ => true
      }} mapConserve classArgTransform
      classArgs.copy(args = newArgs)
    }
  }
}
