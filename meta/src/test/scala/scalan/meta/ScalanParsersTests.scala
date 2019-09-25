package scalan.meta

import scalan.meta.ScalanAst._
import scalan.meta.Symbols.SEntityDefSymbol

class ScalanParsersTests extends BaseMetaTests with Examples {

  import scalan.meta.ScalanAst.{STraitCall => TC, SUnitDef => EMD, SClassDef => CD, STpeTuple => T, SMethodArg => MA, STraitDef => TD, SMethodDef => MD, SMethodArgs => MAs, SImportStat => IS}
  import scala.{List => L}
  val cols = parseModule(colsVirtModule)
  val eCollection = cols.getEntity("Collection")
  val us = cols.unitSym

  describe("STpeExpr") {
    implicit val ctx = new ParseCtx(true)
    testSTpe(us, "Int", TpeInt)
    testSTpe(us, "(Int,Boolean)", STpeTuple(L(TpeInt, TpeBoolean)))
    testSTpe(us, "Int=>Boolean", STpeFunc(TpeInt, TpeBoolean))
    testSTpe(us, "Int=>Boolean=>Float", STpeFunc(TpeInt, STpeFunc(TpeBoolean, TpeFloat)))
    testSTpe(us, "(Int,Boolean=>Float)", STpeTuple(L(TpeInt, STpeFunc(TpeBoolean, TpeFloat))))
    testSTpe(us, "(Int,(Boolean=>Float))", STpeTuple(L(TpeInt, STpeFunc(TpeBoolean, TpeFloat))))
    testSTpe(us, "(Int,Boolean)=>Float", STpeFunc(STpeTuple(L(TpeInt, TpeBoolean)), TpeFloat))
    testSTpe(us, "Edge", TC("Edge", Nil))
    testSTpe(us, "Edge[V,E]", TC("Edge", L(TC("V", Nil), TC("E", Nil))))
    testSTpe(us, "Ref[A=>B]", TC("Ref", L(STpeFunc(TC("A", Nil), TC("B", Nil)))))
  }

  describe("SMethodDef") {
    implicit val ctx = new ParseCtx(true)
    val es = eCollection.symbol
    testSMethod(es, "def f: Int", MD(es, "f", Nil, Nil, Some(TpeInt), false, false, None, Nil, None))
    testSMethod(es, "@OverloadId(\"a\") implicit def f: Int", MD(es, "f", Nil, Nil, Some(TpeInt), true, false, Some("a"), L(SMethodAnnotation("OverloadId",Nil,List(SConst("a")))), None))
    testSMethod(es,
      "def f(x: Int): Int",
      MD(es, "f", Nil, L(MAs(List(MA(false, false, "x", TpeInt, None)))), Some(TpeInt), false, false, None, Nil, None))
    testSMethod(es,
      "def f[A <: T](x: A): Int",
      MD(es, "f", L(STpeArg("A", Some(TC("T", Nil)), Nil)), L(MAs(L(MA(false, false, "x", TC("A", Nil), None)))), Some(TpeInt), false, false, None, Nil, None))
    testSMethod(es,
      "def f[A : Numeric]: Int",
      MD(es, "f", L(STpeArg("A", None, L("Numeric"))), Nil, Some(TpeInt), false, false, None, Nil, None))
    testSMethod(es,
      "def f[A <: Int : Numeric : Fractional](x: A)(implicit y: A): Int",
      MD(es, "f",
        L(STpeArg("A", Some(TpeInt), L("Numeric", "Fractional"))),
        L(MAs(L(MA(false, false, "x", TC("A", Nil), None))), MAs(L(MA(true, false, "y", TC("A", Nil), None)))),
        Some(TpeInt), false, false, None, Nil, None))
  }

  describe("TraitDef") {
    implicit val ctx = new ParseCtx(true)
    val traitA = TD(us, "A", Nil, Nil, Nil, None, None)
    val traitEdgeVE = TD(us, "Edge", L(STpeArg("V", None, Nil), STpeArg("E", None, Nil)), Nil, Nil, None, None)

    testTrait("trait A", traitA)
    testTrait("trait A extends B",
      traitA.copy(ancestors = L(TC("B", Nil).toTypeApply)))
    testTrait("trait A extends B with C",
      traitA.copy(ancestors = L(TC("B", Nil).toTypeApply, TC("C", Nil).toTypeApply)))
    testTrait("trait Edge[V,E]", traitEdgeVE)
    testTrait("trait Edge[V,E]{}", traitEdgeVE)
    testTrait("trait Edge[V,E]{ def f[A <: T](x: A, y: (A,T)): Int }",
      traitEdgeVE.copy(
        body = L(MD(traitEdgeVE.symbol, "f", L(STpeArg("A", Some(TC("T", Nil)), Nil)),
          L(MAs(L(MA(false, false, "x", TC("A", Nil), None), MA(false, false, "y", T(L(TC("A", Nil), TC("T", Nil))), None)))),
          Some(TpeInt), false, false, None, Nil, None))))
    val ts = SEntityDefSymbol(us, "A")
    testTrait(
      """trait A {
        |  import scalan._
        |  type Ref[A] = A
        |  def f: (Int,A)
        |  @OverloadId("b")
        |  def g(x: Boolean): A
        |}""".stripMargin,
      TD(us, "A", Nil, Nil, L(
        IS("scalan._"),
        STpeDef(ts, "Ref", L(STpeArg("A", None, Nil)), TC("A", Nil)),
        MD(ts, "f", Nil, Nil, Some(T(L(TpeInt, TC("A", Nil)))), false, false, None, Nil, None),
        MD(ts, "g", Nil, L(MAs(L(MA(false, false, "x", TpeBoolean, None)))), Some(TC("A", Nil)), false, false, Some("b"), L(SMethodAnnotation("OverloadId", Nil, List(SConst("b")))), None)), None, None))

  }

  val reactiveTrait =
    """trait Reactive extends Scalan {
      |  type Obs[A] = Ref[Observable[A]]
      |  trait Observable[A] {
      |    implicit def eA: Elem[A]
      |  }
      |  class ObservableImpl[A](implicit val eA: Elem[A]) extends Observable[A] {
      |  }
      |}
    """.stripMargin

  describe("SClassDef") {
    implicit val ctx = new ParseCtx(true)
    val classA =
      CD(us, "A", Nil, SClassArgs(Nil), SClassArgs(Nil), Nil, Nil, None, None, false)
    val classEdgeVE =
      CD(us, "Edge", L(STpeArg("V", None, Nil), STpeArg("E", None, Nil)), SClassArgs(Nil), SClassArgs(Nil), Nil, Nil, None, None, false)
    testSClass("class A", classA)
    testSClass("class A extends B",
      classA.copy(ancestors = L(TC("B", Nil).toTypeApply)))
    testSClass("class A extends B with C",
      classA.copy(ancestors = L(TC("B", Nil).toTypeApply, TC("C", Nil).toTypeApply)))
    testSClass("class Edge[V,E]", classEdgeVE)
    testSClass("class Edge[V,E](val x: V){ def f[A <: T](x: A, y: (A,T)): Int }",
      classEdgeVE.copy(
        args = SClassArgs(L(SClassArg(classEdgeVE.symbol, false, false, true, "x", TC("V", Nil), None))),
        body = L(MD(classEdgeVE.symbol, "f", L(STpeArg("A", Some(TC("T", Nil)), Nil)),
          L(MAs(L(MA(false, false, "x", TC("A", Nil), None), MA(false, false, "y", T(L(TC("A", Nil), TC("T", Nil))), None)))),
          Some(TpeInt), false, false, None, Nil, None))))
  }

  describe("SModuleDef") {
    implicit val ctx = new ParseCtx(isVirtualized = true)
    val us = context.newUnitSymbol("scalan.rx", reactiveModule.moduleName)
    val tpeArgA = L(STpeArg("A", None, Nil))
    val ancObsA = L(TC("Observable", L(TC("A", Nil))))
    val entitySym = SEntityDefSymbol(us, "Observable")
    val entity = TD(us, "Observable", tpeArgA, Nil, L(SMethodDef(entitySym, "eA",List(),List(),Some(TC("Elem",L(TC("A",Nil)))),true,false, None, Nil, None, true)), None, None)
    val obsImpl1Sym = SEntityDefSymbol(us, "ObservableImpl1")
    val obsImpl1 = CD(us, "ObservableImpl1", tpeArgA,
      SClassArgs(Nil),
      SClassArgs(L(SClassArg(obsImpl1Sym, true, false, true, "eA", TC("Elem", L(TC("A", Nil))), None, Nil, true))),
      ancObsA.map(_.toTypeApply), Nil, None, None, false)
    val obsImpl2Sym = SEntityDefSymbol(us, "ObservableImpl2")
    val obsImpl2 = CD(us, "ObservableImpl2", tpeArgA,
      SClassArgs(Nil),
      SClassArgs(L(SClassArg(obsImpl2Sym, true, false, true, "eA", TC("Elem", L(TC("A", Nil))), None, Nil, true))),
      ancObsA.map(_.toTypeApply), Nil, None, None, false)

    testModule(reactiveModule,
      EMD("scalan.rx", L(SImportStat("scalan._")), reactiveModule.moduleName,
        List(STpeDef(us, "Obs", L(STpeArg("A",None,Nil)) , TC("Ref", ancObsA))),
        List(entity),
        L(obsImpl1, obsImpl2),
        Nil,
        None,
//        stdDslImpls = Some(SDeclaredImplementations(Map())),
//        expDslImpls = Some(SDeclaredImplementations(Map())),
        ancestors = L(STraitCall("Scalan", Nil).toTypeApply), None, true))
  }
}
