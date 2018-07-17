package scalan.meta

import scalan.meta.ScalanAst.{SArgSection, STraitCall, SClassArg, SLiteralPattern, STpeDef, SWildcardPattern, SUnitDef, SValDef, STypedPattern, STpeArgs, STpeExpr, STpeFunc, SSelPattern, SExpr, STpeArg, STpeEmpty, STuple, SClassDef, SCase, SIf, SIdent, SStableIdPattern, SBindPattern, STpeTuple, SMethodArg, SApplyPattern, SObjectDef, STraitDef, SMethodDef, SAssign, SApply, SFunc, SConst, SEmpty, STypeApply, SBlock, STypeArgAnnotation, SExprApply, SClassArgs, SAscr, SThis, SMethodArgs, SMatch, SPattern, STpePrimitive, SAnnotated, SBodyItem, STpeAnnotated, SEntityDef, SConstr, SSuper, STpeExistential, SSelect, SAltPattern}
import scalan.meta.Symbols.SEntityDefSymbol

object ScalanAstTraversers {

  /** The class implements a default Meta AST transformation strategy: breadth-first search */
  class AstTraverser(implicit val ctx: AstContext) {
    def constTraverse(c: SConst): Unit = {}
    def identTraverse(ident: SIdent): Unit = {}
    def selectTraverse(select: SSelect): Unit = {
      exprTraverse(select.expr)
    }
    def applyTraverse(apply: SApply): Unit = {
      exprTraverse(apply.fun)
      apply.argss foreach (sec => sec.foreach(exprTraverse))
    }
    def tupleTraverse(tuple: STuple): Unit = {
      tuple.exprs foreach exprTraverse
    }
    def exprApplyTraverse(exprApply: SExprApply): Unit = {
      exprTraverse(exprApply.fun)
    }
    def thisTraverse(sthis: SThis): Unit = {}
    def constrTraverse(constr: SConstr): Unit = {
      constr.args foreach exprTraverse
    }
    def ascrTraverse(ascr: SAscr): Unit = {
      exprTraverse(ascr.expr)
    }
    def funcTraverse(func: SFunc): Unit = {
      func.params foreach valdefTraverse
      exprTraverse(func.res)
    }
    def blockTraverse(block: SBlock): Unit = {
      block.init foreach exprTraverse
      exprTraverse(block.last)
    }

    def exprTraverse(expr: SExpr): Unit = expr match {
      case empty: SEmpty => // do nothing
      case c: SConst => constTraverse(c)
      case ident: SIdent => identTraverse(ident)
      case apply: SApply => applyTraverse(apply)
      case exprApply: SExprApply => exprApplyTraverse(exprApply)
      case select: SSelect => selectTraverse(select)
      case constr: SConstr => constrTraverse(constr)
      case sthis: SThis => thisTraverse(sthis)
      case ascr: SAscr => ascrTraverse(ascr)
      case func: SFunc => funcTraverse(func)
      case block: SBlock => blockTraverse(block)
      case bodyItem: SBodyItem => bodyItemTraverse(bodyItem)
      case tup: STuple => tupleTraverse(tup)
      case arg: SClassArg => classArgTraverse(arg)
      case m: SMatch => matchTraverse(m)
      case c: SCase => caseTraverse(c)
      case e: SIf => ifTraverse(e)
      case e: SAssign => assignTraverse(e)
      case e: SAnnotated => annotatedTraverse(e)
      case e: SSuper => superTraverse(e)
      case _ => throw new NotImplementedError(s"Cannot exprTraverse($expr)")
    }

    def tpeExprTraverse(tpe: STpeExpr): Unit = tpe match {
      case tup: STpeTuple => tupleTraverse(tup)
      case func: STpeFunc => funcTraverse(func)
      case empty: STpeEmpty => emptyTraverse(empty)
      case traitCall: STraitCall => traitCallTraverse(traitCall)
      case prim: STpePrimitive => primitiveTraverse(prim)
      case existType: STpeExistential => tpeExistentialTraverse(existType)
      case ann: STpeAnnotated => tpeAnnotatedTraverse(ann)
      case _ =>
        sys.error(s"Don't know how to tpeExprTraverse($tpe)")
    }

    def tpeAnnotatedTraverse(ann: STpeAnnotated): Unit = tpeExprTraverse(ann.tpt)
    def tupleTraverse(tup: STpeTuple): Unit = tup.args.foreach(tpeExprTraverse)
    def funcTraverse(func: STpeFunc): Unit = {
      tpeExprTraverse(func.domain)
      tpeExprTraverse(func.range)
    }
    def emptyTraverse(et: STpeEmpty): Unit = {}
    def primitiveTraverse(prim: STpePrimitive): Unit = {}
    def traitCallTraverse(tc: STraitCall): Unit = {
      tc.args.foreach(tpeExprTraverse)
    }
    def typeArgAnnotationTraverse(ann: STypeArgAnnotation) = {
      ann.tpeArgs.foreach(tpeExprTraverse)
      ann.args.foreach(exprTraverse)
    }
    def tpeArgTraverse(tpeArg: STpeArg): Unit = {
      tpeArg.bound.foreach(tpeExprTraverse)
      tpeArg.tparams.foreach(tpeArgTraverse)
      tpeArg.annotations.foreach(typeArgAnnotationTraverse)
    }
    def tpeExistentialTraverse(e: STpeExistential) = {
      tpeExprTraverse(e.tpt)
      e.items.foreach(bodyItemTraverse)
    }
    def superTraverse(e: SSuper) = {
      e.exprType.foreach(tpeExprTraverse)
    }

    def annotatedTraverse(e: SAnnotated) = {
      exprTraverse(e.expr)
      e.exprType.foreach(tpeExprTraverse)
    }

    def ifTraverse(e: SIf) = {
      exprTraverse(e.cond)
      exprTraverse(e.th)
      exprTraverse(e.el)
      e.exprType.foreach(tpeExprTraverse)
    }

    def assignTraverse(e: SAssign) = {
      exprTraverse(e.left)
      exprTraverse(e.right)
      e.exprType.foreach(tpeExprTraverse)
    }

    def matchTraverse(m: SMatch) = {
      exprTraverse(m.selector)
      m.cases.foreach(caseTraverse)
      m.exprType.foreach(tpeExprTraverse)
    }

    def caseTraverse(c: SCase) = {
      patternTraverse(c.pat)
      exprTraverse(c.guard)
      exprTraverse(c.body)
      c.exprType.foreach(tpeExprTraverse)
    }

    def patternTraverse(p: SPattern) = p match {
      case p: SWildcardPattern => wildcardPatternTraverse(p)
      case p: SLiteralPattern  => literalPatternTraverse(p)
      case p: SStableIdPattern => stableIdPatternTraverse(p)
      case p: SSelPattern   => selPatternTraverse(p)
      case p: SAltPattern   => altPatternTraverse(p)
      case p: STypedPattern => typedPatternTraverse(p)
      case p: SBindPattern  => bindPatternTraverse(p)
      case p: SApplyPattern => applyPatternTraverse(p)
    }

    def wildcardPatternTraverse(arg: SWildcardPattern) = {}
    def literalPatternTraverse(arg: SLiteralPattern) = {}
    def stableIdPatternTraverse(arg: SStableIdPattern) = {}
    def selPatternTraverse(arg: SSelPattern) = {}
    def altPatternTraverse(arg: SAltPattern) = {}
    def typedPatternTraverse(arg: STypedPattern) = {}
    def bindPatternTraverse(arg: SBindPattern) = {}
    def applyPatternTraverse(arg: SApplyPattern) = {}

    def methodArgTraverse(arg: SMethodArg): Unit = {}
    def methodArgsTraverse(args: SMethodArgs): Unit = {
      args.args foreach methodArgTraverse
    }
    def methodArgSectionsTraverse(argSections: List[SMethodArgs]): Unit = {
      argSections foreach methodArgsTraverse
    }
    def methodResTraverse(res: Option[STpeExpr]): Unit =
      res foreach tpeExprTraverse
    def methodBodyTraverse(body: Option[SExpr]): Unit = 
      body foreach exprTraverse
      
    def methodTraverse(method: SMethodDef): Unit = {
      methodArgSectionsTraverse(method.argSections)
      methodResTraverse(method.tpeRes)
      methodBodyTraverse(method.body)
    }
    def valdefTraverse(valdef: SValDef): Unit = {
      exprTraverse(valdef.expr)
    }
    def tpeDefArgTraverse(tpeArg: STpeArg): Unit = {}
    def tpeDefArgsTraverse(tpeArgs: STpeArgs): Unit = {
      tpeArgs foreach tpeDefArgTraverse
    }
    def tpeDefTraverse(tpeDef: STpeDef): Unit = {
      tpeDefArgsTraverse(tpeDef.tpeArgs)
    }

    def bodyItemTraverse(bodyItem: SBodyItem): Unit = bodyItem match {
      case method: SMethodDef => methodTraverse(method)
      case valdef: SValDef => valdefTraverse(valdef)
      case tpedef: STpeDef => tpeDefTraverse(tpedef)
      case _ => throw new NotImplementedError(s"bodyItemTraverse(${bodyItem})")
    }

    def traitCompTraverse(traitComp: STraitDef): Unit = {
      traitComp.body foreach bodyItemTraverse
    }
    def classCompTraverse(classComp: SClassDef): Unit = {
      classComp.body foreach bodyItemTraverse
    }
    def objCompTraverse(obj: SObjectDef): Unit = {
      obj.body foreach bodyItemTraverse
    }

    def entityTraverse(companion: SEntityDef): Unit = {
      companion match {
        case tr: STraitDef => traitCompTraverse(tr)
        case clazz: SClassDef => classCompTraverse(clazz)
        case obj: SObjectDef => objCompTraverse(obj)
        case _ => throw new NotImplementedError(s"entityCompTraverse($companion)")
      }
    }

    def entityAncestorTraverse(anc: STypeApply): Unit = {
      traitCallTraverse(anc.tpe)
      anc.ts foreach exprTraverse
    }

    def entityTpeArgTraverse(tpeArg: STpeArg): Unit = {}

    def traitTraverse(entity: STraitDef): Unit = {
      entity.tpeArgs foreach entityTpeArgTraverse
      entity.body foreach bodyItemTraverse
      entity.companion foreach entityTraverse
      entity.ancestors foreach entityAncestorTraverse
    }

//    def classCompanionTraverse(companion: Option[SEntityDef]): Unit = {
//      companion.foreach {
//        case obj: SObjectDef => objectTraverse(obj.body)
//        case tr: STraitDef => bodyTraverse(tr.body)
//        case c: SClassDef => bodyTraverse(tr.body)
//        case unknown => throw new NotImplementedError(unknown.toString)
//      }
//    }
    def classArgTraverse(classArg: SClassArg): Unit = {
      classArg.default.foreach(exprTraverse)
    }
    def classArgsTraverse(classArgs: SClassArgs): Unit = {
      classArgs.args foreach classArgTraverse
    }
    def classTraverse(c: SClassDef): Unit = {
      c.body foreach bodyItemTraverse
      c.companion foreach[Unit] entityTraverse
      classArgsTraverse(c.args)
      classArgsTraverse(c.implicitArgs)
      c.ancestors foreach entityAncestorTraverse
    }

    def unitTraverse(unit: SUnitDef): Unit = {
      unit.traits foreach traitTraverse
      unit.classes foreach classTraverse
    }
  }

  class EntityUseTraverser(accept: STpeExpr => Unit)(implicit ctx: AstContext) extends AstTraverser {
    override def traitCallTraverse(tc: STraitCall): Unit = {
      accept(tc)
      super.traitCallTraverse(tc)
    }
  }
}
