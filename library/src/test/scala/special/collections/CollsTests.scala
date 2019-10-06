package special.collections

import special.collection.{CollOverArray, ReplColl, PairOfCols, PairColl, CReplColl, Coll}
import org.scalacheck.{Shrink, Gen}
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scalan.{GenConfiguration, RType, RTypeTestUtil, RTypeUtil}

class CollsTests extends PropSpec with PropertyChecks with Matchers with CollGens { testSuite =>
  import Gen._
  import scalan.RType._
  import special.collection._
  import special.collection.ExtensionMethods._

  def checkEquality(left: Coll[_], right: Coll[_]) = {
    left.equals(right) shouldBe true
    right.equals(left) shouldBe true
    right.hashCode() shouldBe left.hashCode()
  }

  val typeGenerationDepth = 5
  val testConfiguration = new GenConfiguration(maxArrayLength = 10)
  def valueGen[T](t: RType[T]): Gen[T] = rtypeValueGen(testConfiguration)(t)

  val testMinSuccess = MinSuccessful(100)
  val typeMinSuccess = MinSuccessful(5)
  val successfulAtLeastOnce = MinSuccessful(1)

  val intCollRtype = CollType(IntType)

  /* Test example:

  property("Some prop") {
    forAll(extendedCollTypeGen(typeGenerationDepth), testMinSuccess) { t: RType[Coll[_]] =>
      forAll(valueGen(t), valueGen(t), typeMinSuccess) { (col1: Coll[_], col2: Coll[_]) =>
      }
    }
  }

   */
  import scala.runtime.ScalaRunTime._

  def arrayEq[T](first: Array[T], second: Array[T]) = {
    (first.length == second.length && first.zip(second).forall(pair => pair._1 == pair._2)) shouldBe true
  }

  def tItem(collType: RType[Coll[_]]): RType[_] = collType.asInstanceOf[RType[_]] match {
    case ct: CollType[a] =>
      ct.tItem
    case rct: ReplCollType[a] => rct.tItem
    case _ => throw new RuntimeException("Not a collection")
  }

  property("Coll.slice") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), indexGen, indexGen, typeMinSuccess) { (col, from, until) =>
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }
  }

  property("Coll.indices") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), typeMinSuccess) { (col1: Coll[_], col2: Coll[_]) =>
        col1.indices.toArray shouldBe col1.toArray.indices.toArray
        col1.zip(col2).length shouldBe math.min(col1.length, col2.length)
        col1.zip(col2).indices.toArray shouldBe col1.toArray.zip(col2.toArray).indices.toArray
      }
    }
  }

  property("Coll.flatMap") {
    def runTest[T](externalCol: Coll[Coll[T]], internalCol: Coll[T])(implicit tC: RType[Coll[T]]) = {
      val matrix = externalCol.map(_ => internalCol)(tC)
      val res = externalCol.zip(matrix)
      val ret = res.flatMap(x => x._2)(tItem(tC.asInstanceOf[RType[Coll[_]]]).asInstanceOf[RType[T]])

      val first = ret.toArray
      val second = externalCol.toArray.flatMap(_ => internalCol.toArray).array.asInstanceOf[Array[T]]
      arrayEq(first, second)
    }
    forAll(arrayTypeGen(collTypeGen(typeGenerationDepth - 1), 1).asInstanceOf[Gen[ArrayType[Coll[_]]]], testMinSuccess) { t =>
      forAll(valueGen(t.tA), valueGen(t), typeMinSuccess) { (col, zsArr) =>
        val arrayItemType = t.tA
        val zs = builder.fromArray(zsArr)(arrayItemType)
        runTest(zs.asInstanceOf[Coll[Coll[Any]]], col.asInstanceOf[Coll[Any]])(arrayItemType.asInstanceOf[RType[Coll[Any]]])
      }
    }
  }

  property("Coll.segmentLength") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tItem(tCol)), valueGen(tCol), indexGen, typeMinSuccess) { (item, col, from) =>
          col.segmentLength(_.equals(item), from) shouldBe col.toArray.segmentLength(_.equals(item), from)
      }
    }

    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), indexGen, typeMinSuccess) { (col, from) =>
        col.segmentLength(collMatchRepl, from) shouldBe col.toArray.segmentLength(collMatchRepl, from)
      }
    }
  }

  property("Coll.indexWhere") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tItem(tCol)), valueGen(tCol), indexGen, typeMinSuccess) { (item, col, from) =>
        col.segmentLength(_.equals(item), from) shouldBe col.toArray.segmentLength(_.equals(item), from)
      }
    }

    forAll(valueGen(intCollRtype), indexGen, testMinSuccess) { (col: Coll[Int], from: Int) =>
      col.indexWhere(eq0, from) shouldBe col.toArray.indexWhere(eq0, from)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe col.toArray.zip(col.toArray).indexWhere(p2, from)
    }
  }

  property("Coll.indexOf") {
    def runTest[T](colItem: T, col: Coll[T], from: Int)(implicit t: RType[T]) = {
      col.indexOf(colItem, from) shouldBe col.toArray.indexOf(colItem, from)
      col.zip(col).indexOf((colItem, colItem), from) shouldBe col.toArray.zip(col.toArray).indexOf((colItem, colItem), from)
    }

    forAll(collTypeGen(typeGenerationDepth - 1), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tItem(tCol)), valueGen(tCol), indexGen, typeMinSuccess) { (item, col, from) =>
        runTest(item, col.asInstanceOf[Coll[Any]], from)(tCol.asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.lastIndexWhere") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tItem(tCol)), valueGen(tCol), indexGen, typeMinSuccess) { (item, col, end) =>
        col.lastIndexWhere(_.equals(item), end) shouldBe col.lastIndexWhere(_.equals(item), end)
      }
    }
    forAll(valueGen(intCollRtype), indexGen, testMinSuccess) { (col: Coll[Int], end: Int) =>
      col.lastIndexWhere(eq0, end) shouldBe col.lastIndexWhere(eq0, end)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).lastIndexWhere(p2, end) shouldBe col.toArray.zip(col.toArray).lastIndexWhere(p2, end)
    }
  }

  property("Coll.partition") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val (lsC, rsC) = col.partition(equals)
        val (ls, rs) = col.toArray.partition(equals)
        lsC.toArray shouldBe ls
        rsC.toArray shouldBe rs
      }
    }
    forAll(valueGen(intCollRtype)) { col =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.toArray.partition(lt0)
      lsC.toArray shouldBe ls
      rsC.toArray shouldBe rs
    }
  }

  property("Coll.patch") {
    def runTest[T](col: Coll[T], patch: Coll[T], from: Int, replaced: Int)(implicit t: RType[T]) = {
      val patchedC = col.patch(from, patch, replaced)
      val patched = col.toArray.patch(from, patch.toArray, replaced)
      arrayEq(patchedC.toArray, patched.array.asInstanceOf[Array[T]])
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), choose(-100, 100), indexGen, typeMinSuccess) { (col, patch, from, replaced) =>
        whenever(from < col.length) {
          runTest(col.asInstanceOf[Coll[Any]], patch.asInstanceOf[Coll[Any]], from, replaced)(tItem(tCol).asInstanceOf[RType[Any]])
        }
      }
    }
  }

 property("Coll.updated") {
    def runTest[T](col: Coll[T], patch: T, index: Int)(implicit t: RType[T]) = {
      if (0 <= index && index < col.length) {
        val patchedC = col.updated(index, patch)
        val patched = col.toArray.updated(index, patch)
        arrayEq(patchedC.toArray, patched.array.asInstanceOf[Array[T]])
      } else {
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(index, patch)
        }
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(-1, patch)
        }
      }
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tItem(tCol)), valueGen(tCol), indexGen, typeMinSuccess) { (patch, testColl, index) =>
        runTest(testColl.asInstanceOf[Coll[Any]], patch.asInstanceOf[Any], index)(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.updateMany") {
    def runTest[T](col: Coll[T], patch: Coll[T], indexes: Coll[Int])(implicit t: RType[T]) = {
      if (indexes.forall(x => x < col.length && x > -1)) {
        val updatedC = col.updateMany(indexes, patch)
        val updated = col.toArray.clone()
        for ((i, value) <- indexes.zip(patch))
          updated.update(i, value)
        updatedC.toArray shouldBe updated
      } else {
        if (col.length > 0) {
          an[IndexOutOfBoundsException] should be thrownBy {
            col.updateMany(builder.fromItems(col.length), col.slice(0, 1))
          }
          an[IndexOutOfBoundsException] should be thrownBy {
            col.updateMany(builder.fromItems(-1), col.slice(0, 1))
          }
        }
      }
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), valueGen(CollType(IntType)), typeMinSuccess) { (col, update, indexes) =>
        runTest(col.asInstanceOf[Coll[Any]],
          update.slice(0,
            if (indexes.length > update.length) update.length else indexes.length
          ).asInstanceOf[Coll[Any]], indexes)(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll methods") {
    val intTunedConf = new GenConfiguration(
      maxArrayLength = testConfiguration.maxArrayLength,
      intBorders = (-100, 100)
    )
    forAll(rtypeValueGen(intTunedConf)(intCollRtype), indexGen) { (col, index) =>
      {
        val res = col.sum(monoid)
        res shouldBe col.toArray.sum
        val pairs = col.zip(col)
        val pairMonoid = builder.Monoids.pairMonoid(monoid, monoid)
        pairs.sum(pairMonoid) shouldBe ((res, res))
      }
      {
        val res = col.map(inc)
        res.toArray shouldBe col.toArray.map(inc)
        val pairs = col.zip(col)
        pairs.map(plusF).toArray shouldBe pairs.toArray.map(plusF)
      }
      {
        val res = col.filter(lt0)
        res.toArray shouldBe col.toArray.filter(lt0)
      }
      {
        val res = col.forall(lt0)
        val emptyRepl = builder.replicate(0, 10)
        val repl = builder.replicate(col.length, 10)
        res shouldBe col.toArray.forall(lt0)
        emptyRepl.forall(lt0) shouldBe Array[Int]().forall(lt0)
        col.zip(repl).forall(predF) shouldBe col.zip(repl).toArray.forall(predF)
      }
      {
        val res = col.exists(lt0)
        res shouldBe col.toArray.exists(lt0)
        builder.replicate(0, -10).exists(lt0) shouldBe Array[Int]().exists(lt0)
      }
      {
        val res = col.foldLeft[Int](0, plusF)
        res shouldBe col.toArray.foldLeft(0)(plus)
        val pairs = col.zip(col)
        val op = (in: (Int,(Int,Int))) => in._1 + in._2._1 + in._2._2
        pairs.foldLeft(0, op) shouldBe pairs.toArray.foldLeft(0)((b,a) => op((b,a)))
      }
      if (index < col.length) {
        val res = col(index)
        res shouldBe col.toArray(index)

        val res2 = col.getOrElse(index, index)
        res2 shouldBe col.toArray(index)
      }

      col.getOrElse(col.length, index) shouldBe index
      col.getOrElse(-1, index) shouldBe index
    }
  }

  property("Coll.apply") {
    def runTest[T](col: Coll[T], index: Int)(implicit t: RType[T]) = {
      if (index < col.length) {
        val res = col(index)
        res shouldBe col.toArray(index)
      } else {
        an[IndexOutOfBoundsException] should be thrownBy {
          col(index)
        }
      }
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), indexGen, typeMinSuccess) { (col, index) =>
          runTest(col.asInstanceOf[Coll[Any]], index)(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.append") {
    def runTest[T](col1: Coll[T], col2: Coll[T])(implicit t: RType[T]) = {
      val res = col1.append(col2)
      arrayEq(res.toArray, (col1.toArray ++ col2.toArray).array.asInstanceOf[Array[T]])
      val pairs1 = col1.zip(col1)
      val pairs2 = col2.zip(col2)
      val apairs = pairs1.append(pairs2)
      arrayEq(apairs.toArray, (pairs1.toArray ++ pairs2.toArray))
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), typeMinSuccess) { (col1, col2) =>
        val collItemType = tItem(tCol)
        runTest(col1.asInstanceOf[Coll[Any]], col2.asInstanceOf[Coll[Any]])(collItemType.asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.mapReduce") {
    import scalan.util.CollectionUtil.TraversableOps
    def m[T](x: T) = (x.hashCode() % 10, x)
    def projectionSnd[T](x: (T, T)): T = x._2
    def takeSnd[T](x1: T, x2:  T): T = x2
    def runTest[T](col: Coll[T])(implicit t: RType[T]) = {
      val res = col.mapReduce(m, projectionSnd[T])(IntType, t.asInstanceOf[RType[T]])
      val (ks, vs) = builder.unzip(res)
      ks.length <= 10 shouldBe true
      res.toArray shouldBe col.toArray.toIterable.mapReduce(m)(takeSnd[T]).toArray
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val collItemType = tItem(tCol)
        runTest(col.asInstanceOf[Coll[Any]])(collItemType.asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.groupBy") {
    def runTest[T](col: Coll[T])(implicit t: RType[T]) = {
      val res = col.groupBy(key)
      val (ks, vs) = builder.unzip(res)
      vs.flatten.map(key).toArray.sum shouldBe col.toArray.map(key).sum
      ks.length <= 10 shouldBe true
      val pairs = col.map(x => (key(x), x))(pairRType(IntType, col.tItem))
      val res2 = pairs.groupByKey
      val (ks2, vs2) = builder.unzip(res2)
      ks shouldBe ks2
      vs shouldBe vs2
    }
    def key[T](x: T) = x.hashCode() % 10
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val collItemType = tItem(tCol)
        runTest(col.asInstanceOf[Coll[Any]])(collItemType.asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.reverse") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val res = col.reverse
        res.toArray shouldBe col.toArray.reverse
        val pairs = col.zip(col)
        pairs.reverse.toArray shouldBe pairs.toArray.reverse

        val c1 = col.asInstanceOf[Coll[Any]]
        val appended = c1.append(c1)
        appended.toArray shouldBe (c1.toArray ++ c1.toArray)
      }
    }
  }

  property("Coll.take") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val n = col.length / 2
        val res = col.take(n)
        res.toArray shouldBe col.toArray.take(n)
        val pairs = col.zip(col)
        pairs.take(n).toArray shouldBe pairs.toArray.take(n)
      }
    }
  }

  property("Coll.distinct") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val res = col.distinct
        res.toArray shouldBe col.toArray.distinct
        val pairs = col.zip(col)
        pairs.distinct.toArray shouldBe pairs.toArray.distinct
      }
    }
  }

  property("Coll.equals") {
    def checkColls(repl: Coll[_], coll: Coll[_]) = {
      assert(coll == repl)
      assert(repl == coll)
      repl.hashCode() shouldBe coll.hashCode()

      val zip1 = repl.zip(repl)
      val zip2 = repl.zip(coll)
      val zip3 = coll.zip(coll)
      val zip4 = coll.zip(repl)

      assert(zip1 == zip2)
      assert(zip2 == zip3)
      assert(zip3 == zip4)
      assert(zip4 == zip1)
      zip1.hashCode() shouldBe zip2.hashCode()
      zip2.hashCode() shouldBe zip3.hashCode()
      zip3.hashCode() shouldBe zip4.hashCode()
      zip4.hashCode() shouldBe zip1.hashCode()
    }
    forAll(rtypeGen(typeGenerationDepth), testMinSuccess) { t: RType[_] =>
      forAll(valueGen(t), indexGen, typeMinSuccess) { (item, n) =>
        val repl = new CReplColl(item.asInstanceOf[Any], n)(t.asInstanceOf[RType[Any]])
        val coll = new CollOverArray(Array.fill(n)(item.asInstanceOf[Any]))(t.asInstanceOf[RType[Any]])
        checkColls(repl, coll)
      }
    }
    // TODO: Test with builder
  }

  property("PairColl.mapFirst") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { col =>
        val pairs = col.zip(col)
        pairs.mapFirst(hashCodeInc).toArray shouldBe pairs.toArray.map { case (x, y) => (hashCodeInc(x), y) }
        pairs.mapSecond(hashCodeInc).toArray shouldBe pairs.toArray.map { case (x, y) => (x, hashCodeInc(y)) }
      }
    }
  }

  property("Coll.unionSet") {
    def runTest[T](col1: Coll[T], col2: Coll[T])(implicit t: RType[T]) = {
      val res = col1.unionSet(col2)
      arrayEq(res.toArray, (col1.toArray.union(col2.toArray).distinct).array.asInstanceOf[Array[T]])
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), typeMinSuccess) { (col1, col2) =>
        runTest(col1.asInstanceOf[Coll[Any]], col2.asInstanceOf[Coll[Any]])(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.diff") {
    def runTest[T](col1: Coll[T], col2: Coll[T])(implicit t: RType[T]) = {
      val res = col1.diff(col2)
      res.toArray shouldBe (col1.toArray.diff(col2.toArray))
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), typeMinSuccess) { (col1, col2) =>
        runTest(col1.asInstanceOf[Coll[Any]], col2.asInstanceOf[Coll[Any]])(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("Coll.intersect") {
    def runTest[T](col1: Coll[T], col2: Coll[T])(implicit t: RType[T]) = {
      val res = col1.intersect(col2)
      res.toArray shouldBe (col1.toArray.intersect(col2.toArray))
    }
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), valueGen(tCol), typeMinSuccess) { (col1, col2) =>
        runTest(col1.asInstanceOf[Coll[Any]], col2.asInstanceOf[Coll[Any]])(tItem(tCol).asInstanceOf[RType[Any]])
      }
    }
  }

  property("CollBuilder.xor") {
    def runTest[T](col1: Coll[T], col2: Coll[T])(implicit t1: RType[T]) = {
      val n = if (col1.length < col2.length) col1.length else col2.length
      val c1 = col1.take(n).asInstanceOf[Coll[Byte]]
      val c2 = col2.take(n).asInstanceOf[Coll[Byte]]
      builder.xor(c1, c2).toArray shouldBe c1.toArray.zip(c2.toArray).map { case (l, r) => (l ^ r).toByte }
    }
    forAll(valueGen(CollType(ByteType)), valueGen(ReplCollType(ByteType)), typeMinSuccess) { (col1, col2) =>
      runTest(col1, col1)(ByteType)
      runTest(col1, col2)(ByteType)
      runTest(col2, col1)(ByteType)
      runTest(col2, col2)(ByteType)
    }
  }

  property("CollBuilder.outerJoin") {
    def test(col: Coll[Int]) = {
      val inner = col.indices
      val rightOnly = inner.map(i => i + col.length)
      val leftOnly = rightOnly.map(i => -i)

      val leftKeys = inner.append(leftOnly)
      val leftValues = col.append(col.map(x => x + 2))

      val rightKeys = inner.append(rightOnly)
      val rightValues = col.append(col.map(x => x + 3))

      val left  = builder.pairColl(leftKeys, leftValues)
      val right = builder.pairColl(rightKeys, rightValues)
      val res = builder.outerJoin(left, right)(l => l._2 - 2, r => r._2 - 3, i => i._2._1 + 5)
      val (ks, vs) = builder.unzip(res)
      vs.sum(monoid) shouldBe (col.sum(monoid) * 2 + col.map(_ + 5).sum(monoid))
    }
    val intTunedConf = new GenConfiguration(
      maxArrayLength = testConfiguration.maxArrayLength,
      intBorders = (-100, 100)
    )
    forAll(rtypeValueGen(intTunedConf)(intCollRtype)) { (col) =>
      test(col)
    }
  }

  property("CViewColl.correctWork") {
    forAll(collTypeGen(typeGenerationDepth), testMinSuccess) { tCol: RType[Coll[_]] =>
      forAll(valueGen(tCol), typeMinSuccess) { (col) =>
        val view = builder.makeView(col, hashCodeInc)
        val usual = col.map(hashCodeInc)
        view.toArray shouldBe usual.toArray
      }
    }
  }

  property("CollOverArray tuple array construction") {
    forAll (intGen, indexGen) { (i, n) =>
      val replArr = Array.fill(n)((i, i))
      an [RuntimeException] should be thrownBy new CollOverArray(replArr)
    }
  }

  // TODO: improve ViewColl and CollOverArray equality with complex data
  ignore("ViewColl vs CollOverArray complex equality") {
    forAll(indexGen, intGen) { (n, item) =>
      def f(i: Int): (Int, Int) = (i + 10, i - 10)
      val view = builder.makeView(builder.replicate(n, item), f)
      val repl = builder.replicate(n, item).map(f)

      checkEquality(view, repl)
    }
  }

  property("CViewColl.equality") {
    forAll(indexGen, intGen) { (n, item) =>
      def f(i: Int): Int = i + 10
      val view = builder.makeView(builder.replicate(n, item), f)
      val repl = builder.replicate(n, item).map(f)
      checkEquality(view, repl)

      val newView = builder.makeView(builder.makeView(view, inc), dec)
      checkEquality(newView, view)
      checkEquality(newView, repl)
    }
    forAll(collOverArrayGen) { coll =>
      def f(i: Int): Int = i * 10
      val view = builder.makeView(coll, f)
      val mapped = coll.map(f)
      checkEquality(view, mapped)
    }
    forAll (byteGen, doubleGen, intGen, indexGen) { (b, d, i, n) =>
      val repl = builder.replicate(n, (b, i))
      val view = builder.makeView(repl, (t: (Byte, Int)) => ((t._1 / 2).toByte, t._2 * 2))
      view.equals(repl.map((t: (Byte, Int)) => ((t._1 / 2).toByte, t._2 * 2))) shouldBe true
    }
  }

  property("Coll equality") {
    val arr1 = Array[Int](1, 2, 3)
    val arr2 = Array[Int](1, 2, 3)
    val repl1 = Array[Int](1,1,1)
    val repl2 = Array[Int](1,1,1)
    val pairs1 = arr1.zip(repl1)
    val replPairs = repl1.zip(repl2)
    def ids = Array.tabulate(3) { i => Array.fill(32)(i.toByte) }
    def replIds = Array.fill(3) { Array.fill(32)(1) }
    val tokensArr = ids.zip(arr1)
    case class NoShrink[T](x: T)

    val collGen = Gen.oneOf(builder.fromArray(arr1), builder.fromArray(arr2), builder.fromItems(1, 2, 3)).map(NoShrink(_))
    val replGen = Gen.oneOf(builder.fromArray(repl1), builder.replicate(3, 1)).map(NoShrink(_))
    val idsGen = Gen.oneOf(builder.fromArray(ids), builder.fromArray(ids)).map(NoShrink(_))
    val replIdsGen = Gen.oneOf(builder.fromArray(replIds), builder.replicate(3, Array.fill(32)(1))).map(NoShrink(_))

    val pairsGen = Gen.oneOf(
      for { c1 <- collGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.fromArray(pairs1)),
      Gen.const(builder.fromItems((1, 1), (2, 1), (3, 1)))
    ).map(NoShrink(_))
    val replPairsGen = Gen.oneOf(
      for { c1 <- replGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.replicate(3, (1,1))),
      Gen.const(builder.fromArray(replPairs))
    ).map(NoShrink(_))

    val tokensGen = Gen.oneOf(
      for { c1 <- idsGen; c2 <- collGen } yield builder.pairColl(c1.x, c2.x): Coll[(Array[Byte], Int)],
      Gen.const(builder.fromArray(tokensArr)),
      Gen.const(builder.fromItems(tokensArr(0), tokensArr(1), tokensArr(2)))
    ).map(NoShrink(_))

    val minSuccess = MinSuccessful(30)

    forAll(collGen, collGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(collGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x != c2.x)
      assert(c2.x != c1.x)
    }
    forAll(pairsGen, pairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replPairsGen, replPairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(idsGen, idsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(replIdsGen, replIdsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(tokensGen, tokensGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

// TODO the following test fails because equality of Seq is not deep, and nested arrays are shallow compared
//    forAll(tokensGen, minSuccess) { c1 =>
//      println(s"c1=${c1.x.toArray.toSeq.map { case (id, v) => (id.toSeq, v) }}")
//      val tokens = c1.x
//      assert(tokens.toArray.toSeq == tokensArr.toSeq)
//    }
  }

}
