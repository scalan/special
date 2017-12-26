package scalan.meta

import scalan.meta.ScalanAst._

class EntityTests extends BaseMetaTests with Examples {

  val cols = parseModule(colsVirtModule)
  context.addModule(cols)
  val eCollection = cols.getEntity("Collection")
  val eColOverArray = cols.getEntity("ColOverArray")
  val ePairCollection = cols.getEntity("PairCollection")
  val ePairOfCols = cols.getEntity("PairOfCols")

  describe("SEntityDef ops") {
    it("getInheritedTypes") {
      val types = eColOverArray.getInheritedTypes
      types.map(_.name) shouldBe(List("Collection", "Def"))
    }
    it("inherits") {
      eColOverArray.inherits("Def") shouldBe(true)
      eColOverArray.inherits("Def1") shouldBe(false)
      eColOverArray.inherits("Collection") shouldBe(true)
      eColOverArray.inherits("Iters") shouldBe(false)
    }
    it("collectMethodsInBody") {
      ePairOfCols.collectItemsInBody(_.isAbstract).map(_.item.name) shouldBe(List())
      ePairOfCols.collectItemsInBody(!_.isAbstract).map(_.item.name) shouldBe(List("ls", "rs", "eL", "eR", "length", "apply", "map"))
    }
    it("collectMethodsFromAncestors") {
      ePairOfCols.collectMethodsFromAncestors(_.isAbstract).map(_.item.name) shouldBe(List("eL", "eR", "ls", "rs", "eA", "arr", "length", "apply", "map", "eA", "arr", "length", "apply", "map"))
      ePairOfCols.collectMethodsFromAncestors(!_.isAbstract).map(_.item.name) shouldBe(List())
    }
    it("argsSubstOfAncestorEntities") {
      val subst = ePairOfCols.argsSubstOfAncestorEntities.map { case (e, args) =>
        (e.name, args.map { case (a, t) => (a.name, t.toString) })
      }
      subst shouldBe List(
        ("PairCollection", List(("L","L"), ("R","R"))),
        ("Collection", List(("A","(L, R)"))))
    }
  }

  describe("linearization") {
    // see http://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#class-linearization
    it("SEntityDef.linearization") {
      ePairCollection.linearization.map(_.name) shouldBe(List("PairCollection", "Collection"))
      ePairOfCols.linearization.map(_.name) shouldBe(List("PairOfCols", "PairCollection", "Collection"))
      eColOverArray.linearization.map(_.name) shouldBe(List("ColOverArray", "Collection"))
    }

    def testRefinementProp(entC: SEntityDef, entD: SEntityDef, anyLin: List[SEntityDef]): Unit = {
      assert(entC.inherits(entD.name))
      val posC = anyLin.map(_.name).indexOf(entC.name)
      val posD = anyLin.map(_.name).indexOf(entD.name)
      assert(posC < posD, "if C is a subclass of D, then C precedes D in any linearization where both C and D occur")
    }

    it("refinement property") {
      testRefinementProp(ePairCollection, eCollection, ePairCollection.linearization)
      testRefinementProp(ePairCollection, eCollection, ePairOfCols.linearization)
    }

    // TODO linearization suffix property test
    // assert(true, "A linearization of a class always contains the linearization of its direct superclass as a suffix.")

    def testLinWithSubst(e: SEntityDef, subst: STpeSubst, expected: List[(String, List[String])]): Unit = {
      val res = e.linearizationWithSubst(subst).map { case (e, args) => (e.name, args.map(_.toString)) }
      res shouldBe(expected)
    }

    it("SEntityDef.linearizationWithSubst") {
      testLinWithSubst(eCollection, Map(), List(("Collection", List("A"))))
      testLinWithSubst(eCollection, Map("A" -> TpeInt), List(("Collection", List("Int"))))
      testLinWithSubst(eColOverArray, Map(), List(("ColOverArray", List("A")), ("Collection", List("A"))))
      testLinWithSubst(eColOverArray, Map("A" -> TpeInt),
        List(("ColOverArray", List("Int")), ("Collection", List("Int"))))
      testLinWithSubst(ePairCollection, Map(),
        List(("PairCollection", List("L", "R")), ("Collection", List("(L, R)"))))
      testLinWithSubst(ePairCollection, Map("L" -> TpeInt),
        List(("PairCollection", List("Int", "R")), ("Collection", List("(Int, R)"))))
      testLinWithSubst(ePairOfCols, Map(),
        List(("PairOfCols", List("L", "R")), ("PairCollection", List("L", "R")), ("Collection", List("(L, R)"))))
      testLinWithSubst(ePairOfCols, Map("L" -> TpeInt),
        List(("PairOfCols", List("Int", "R")), ("PairCollection", List("Int", "R")), ("Collection", List("(Int, R)"))))
    }
  }

  describe("SEntityMember") {
    def testMatches(e1: SEntityDef, e2: SEntityDef, name: String): Unit = {
      val l1 = e1.findMemberInBody(name).get
      val l2 = e2.findMemberInBody(name).get
      assert(l1.matches(l2), s"Method '$name' don't match in entities ${e1.name} and ${e2.name}")
    }

    it("matches") {
      testMatches(ePairOfCols, eCollection, "length")
      testMatches(ePairOfCols, eCollection, "apply")
      testMatches(eColOverArray, eCollection, "map")
      testMatches(eColOverArray, eCollection, "eA")
      testMatches(ePairOfCols, ePairCollection, "eL")
      testMatches(ePairOfCols, eCollection, "map")
    }
  }

  describe("STpeExpr") {
    implicit val parseCtx = new ParseCtx(false)

    def testNames(tpeStr: String, expected: Set[String]) = {
      val t = parseType(tpeStr)
      t.names shouldBe expected
    }

    it("names") {
      testNames("A", Set("A"))
      testNames("(A, B)", Set("A", "B"))
      testNames("(A => B)", Set("A", "B"))
      testNames("Array[A]", Set("Array", "A"))
      testNames("Col[A]", Set("Col", "A"))
    }

    it("getUniqueName") {
      getUniqueName("A", Set("A")) shouldBe "A1"
      getUniqueName("A", Set("A1")) shouldBe "A"
      getUniqueName("A", Set("A", "A1")) shouldBe "A2"
      getUniqueName("A", Set("A2","A")) shouldBe "A1"
    }
  }
}
