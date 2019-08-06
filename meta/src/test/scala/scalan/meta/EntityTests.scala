package scalan.meta

import scalan.meta.ScalanAst._

class EntityTests extends BaseMetaTests with Examples {

  val cols = parseModule(colsVirtModule)
  context.addUnit(cols)
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
      val subst = ePairOfCols.collectAncestorEntitiesWithSubst.map { case (e, args) =>
        (e.name, args.map { case (a, t) => (a.name, t.toString) })
      }
      subst shouldBe List(
        ("PairCollection", List(("L","L"), ("R","R"))),
        ("Collection", List(("A","(L, R)"))))
    }
    describe("collectVisibleMembers") {
      implicit val parseCtx = new ParseCtx(true)
      val us = cols.unitSym
      def testVisible(e: SEntityDef, p: SEntityMember => Boolean, expected: List[(String, String)]) = {
        val expectedMembers = expected.map { case (en, mstr) =>
          val es = cols.getEntity(en).symbol
          val item = parseBodyItem(es, mstr).asInstanceOf[SEntityItem]
          (en, item)
        }
        val actual = e.collectVisibleMembers.filter(p).map(m => (m.entity.name, m.item))
        actual shouldBe expectedMembers
      }
      def isMap(m: SEntityMember) = m.item.name == "map"
      it("returns matching member with specialized signature") {
        testVisible(eCollection, isMap,
          List(("Collection", "def map[B](f: Ref[A => B]): Ref[Collection[B]]")))
        testVisible(eColOverArray, isMap,
          List(("ColOverArray", "def map[B](f: Ref[A => B]): Ref[Collection[B]] = ColOverArray(ColOverArray.this.arr.map(f))")))
        testVisible(ePairCollection, isMap,
          List(("Collection", "def map[B](f: Ref[((L,R)) => B]): Ref[Collection[B]]")))
        testVisible(ePairOfCols, isMap,
          List(("PairOfCols", "override def map[V](f: Ref[((L, R)) => V]): Ref[Collection[V]] = ColOverArray(PairOfCols.this.arr.map(f))")))
      }
      def testMemberEntity(e: SEntityDef, expected: List[(String, String)]) = {
        val actual = e.collectVisibleMembers.map(m => (m.entity.name, m.item.name))
        actual shouldBe expected
      }
      it("returns the most specific version") {
        testMemberEntity(eCollection,
          List(("Collection","eA"), ("Collection","arr"), ("Collection","length"), ("Collection","apply"), ("Collection","map")))
        testMemberEntity(eColOverArray,
          List(("ColOverArray","arr"), ("ColOverArray","eA"), ("ColOverArray","list"), ("ColOverArray","length"), ("ColOverArray","apply"), ("ColOverArray","map")))
        testMemberEntity(ePairCollection,
          List(
            ("PairCollection","eL"), ("PairCollection","eR"), ("PairCollection","ls"), ("PairCollection","rs"),
            ("Collection","eA"), ("Collection","arr"), ("Collection","length"), ("Collection","apply"), ("Collection","map")))
        testMemberEntity(ePairOfCols,
          List(
            ("PairOfCols","ls"), ("PairOfCols","rs"), ("PairOfCols","eL"), ("PairOfCols","eR"),
            ("PairOfCols","length"), ("PairOfCols","apply"), ("PairOfCols","map"),
            ("Collection","eA"), ("Collection","arr")))
      }
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
  implicit val parseCtx = new ParseCtx(false)
  val us = cols.unitSym
  describe("STpeExpr") {
    def testNames(tpeStr: String, expected: Set[String]) = {
      val t = parseType(us, tpeStr)
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

    it("disambiguateNames") {
      disambiguateNames(List("A","B"), Map()) shouldBe(Map())
      disambiguateNames(
        List("A","B"),
        Map("A" -> STpeTuple(List(STraitCall("B"),
        STraitCall("C"))))) shouldBe(Map("A" -> STraitCall("A1"), "B" -> STraitCall("B1")))
    }

    def testSubst(tpeStr: String, subst: STpeSubst, expected: String) = {
      val t = parseType(us, tpeStr)
      t.applySubst(subst).toString shouldBe expected
    }
    it("applySubst") {
      testSubst("(A,B)", Map("A" -> TpeInt), "(Int, B)")
    }
  }

  describe("SEntityItem") {
    def testSubst(tpeStr: String, subst: STpeSubst, expectedStr: String) = {
      val t = parseBodyItem(us, tpeStr)
      val expected = parseBodyItem(us, expectedStr)
      t.asInstanceOf[SEntityItem].applySubst(subst) shouldBe expected
    }
    it("applySubst") {
      def ty(s: String) = parseType(us, s)
      val method = "def map[B](f: A => B): Col[B]"
      testSubst(method, Map("A" -> TpeInt), "def map[B](f: Int => B): Col[B]")
      testSubst(method, Map("A" -> ty("(A,B)")), "def map[B](f: (A,B) => B): Col[B]")
      testSubst(method, Map("A" -> ty("(A,B)"), "B" -> ty("V")), "def map[B](f: (A,B) => V): Col[V]")
      val field = "val col: Col[A]"
      testSubst(field, Map(), "val col: Col[A]")
      testSubst(field, Map("A" -> ty("B")), "val col: Col[B]")
      testSubst(field, Map("A" -> ty("(B,C)")), "val col: Col[(B,C)]")
      testSubst(field, Map("A" -> ty("Col[B]")), "val col: Col[Col[B]]")
      testSubst(field, Map("A" -> ty("Col[(B,C)]")), "val col: Col[Col[(B,C)]]")
      val arg = SClassArg(eCollection.symbol, false, false, true, "arg", ty("(A,B)"), None)
      arg.applySubst(Map("A" -> ty("B"))) shouldBe arg.copy(tpe = ty("(B,B)"))
      arg.applySubst(Map("A" -> ty("(B,C)"))) shouldBe arg.copy(tpe = ty("((B,C),B)"))
    }
  }
}
