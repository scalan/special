package special.collections

import special.collection.{Coll, PairColl, ReplColl}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scalan.RType

class CollsTests extends PropSpec with PropertyChecks with Matchers with CollGens { testSuite =>
  import Gen._
  import special.collection.ExtensionMethods._

  property("Coll.indices") {
    val minSuccess = MinSuccessful(30)
    forAll(collGen, collGen, minSuccess) { (col1: Coll[Int], col2: Coll[Int]) =>
      col1.indices.toArray shouldBe col1.toArray.indices.toArray
//      col1.zip(col2).length shouldBe math.min(col1.length, col2.length)
// TODO     col1.zip(col2).indices.arr shouldBe col1.arr.zip(col2.arr).indices.toArray
    }
    forAll(superGen, minSuccess) {
      case cl: PairColl[_, _] => {
        cl.indices.toArray shouldBe cl.toArray.indices.toArray
      }
      case _ => false shouldBe true
    }
  }

  property("Coll.flatMap") {
    forAll(containerOfN[Coll, Int](3, valGen), collGen) { (zs, col) =>
      val matrix = zs.map(_ => col)
      val res = zs.zip(matrix).flatMap(_._2)
      res.toArray shouldBe zs.toArray.flatMap(_ => col.toArray)
    }
  }

  property("Coll.segmentLength") {
    forAll(collGen, indexGen) { (col, from) =>
      col.segmentLength(lt0, from) shouldBe col.toArray.segmentLength(lt0, from)
    }

    val minSuccess = minSuccessful(30)
    forAll(superGen, indexGen, minSuccess) { (col, from) =>
      col.segmentLength(collMatchRepl, from) shouldBe col.toArray.segmentLength(collMatchRepl, from)
    }
  }

  property("Coll.indexWhere") {
    forAll(collGen, indexGen) { (col, from) =>
      col.indexWhere(eq0, from) shouldBe col.toArray.indexWhere(eq0, from)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe col.toArray.zip(col.toArray).indexWhere(p2, from)
    }
  }

  property("Coll.indexOf") {
    forAll(collGen, indexGen, valGen) { (col, from, elem) =>
      col.indexOf(elem, from) shouldBe col.toArray.indexOf(elem, from)
      col.zip(col).indexOf((elem, elem), from) shouldBe col.toArray.zip(col.toArray).indexOf((elem, elem), from)
    }
  }

  property("Coll.lastIndexWhere") {
    forAll(collGen, indexGen) { (col, end) =>
      col.lastIndexWhere(eq0, end) shouldBe col.lastIndexWhere(eq0, end)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).lastIndexWhere(p2, end) shouldBe col.toArray.zip(col.toArray).lastIndexWhere(p2, end)
    }
  }

  property("Coll.partition") {
    forAll(collGen) { col =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.toArray.partition(lt0)
      lsC.toArray shouldBe ls
      rsC.toArray shouldBe rs
    }
/*
    val minSuccess = minSuccessful(100)
    forAll(superGen, minSuccess) { col =>
      col match {
        case cl: PairColl[_, ReplColl[RType[_]]] => {
          val (lsC, rsC) = cl.partition(collMatchRepl)
          val (ls, rs) = cl.toArray.partition(collMatchRepl)
          for (item <- ls)
            print(item)
          println(lsC)

          println(rsC)
          for (item <- rs)
            print(item)
          rsC shouldBe rs
        }
        case _ => false shouldBe true
      }
    }*/
  }

  property("Coll.patch") {
    forAll(collGen, choose(-100, 100), collGen, replacedGen) { (col, from, patch, replaced) =>
      whenever(from < col.length ) {
        val patchedC = col.patch(from, patch, replaced)
        val patched = col.toArray.patch(from, patch.toArray, replaced)
        patchedC.toArray shouldBe patched
      }
    }
  }

  property("Coll.updated") {
    forAll(collGen, indexGen, valGen) { (col, index, elem) =>
      whenever(index < col.length ) {
        val patchedC = col.updated(index, elem)
        val patched = col.toArray.updated(index, elem)
        patchedC.toArray shouldBe patched
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updated(col.length, elem)
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updated(-1, elem)
      }
    }
  }

  property("Coll.updateMany") {
    forAll(collGen, indexesGen) { (col, indexes) =>
      whenever(indexes.forall(_ < col.length)) {
        val updatedC = col.updateMany(indexes, indexes)
        val updated = col.toArray.clone()
        for (i <- indexes)
          updated.update(i, i)
        updatedC.toArray shouldBe updated
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updateMany(builder.fromItems(col.length), builder.fromItems(0))
      }      
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updateMany(builder.fromItems(-1), builder.fromItems(0))
      }
    }
  }

  property("Coll methods") {
    forAll(collGen, indexGen) { (col, index) =>
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
      whenever(index < col.length) {
        val res = col(index)
        res shouldBe col.toArray(index)

        val res2 = col.getOrElse(index, index)
        res2 shouldBe col.toArray(index)
      }
      
      col.getOrElse(col.length, index) shouldBe index
      col.getOrElse(-1, index) shouldBe index
    }
    forAll(superGen, indexGen) { (col, index) => col match {
      case cl: PairColl[_, _] => {
        whenever(index < col.length) {
          val res = col(index)
          res shouldBe col.toArray(index)
        }
      }
      case _ => false shouldBe true
      }
    }
  }

  property("Coll.slice") {
    forAll(collGen, indexGen, indexGen) { (col, from, until) =>
      whenever(until < col.length) {
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }

    forAll(superGen, indexGen, indexGen) { (col, from, until) =>
      col match {
        case cl: PairColl[_, _] => {
          whenever(until < cl.length) {
            val res = cl.slice(from, until)
            res.toArray shouldBe cl.toArray.slice(from, until)
          }
        }
        case _ => false shouldBe true
      }
    }
  }

  property("Coll.append") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.append(col2)
      res.toArray shouldBe (col1.toArray ++ col2.toArray)
    }
  }

  property("Coll.mapReduce") {
    import scalan.util.CollectionUtil.TraversableOps
    def m(x: Int) = (math.abs(x) % 10, x)
    forAll(collGen) { col =>
      val res = col.mapReduce(m, plusF)
      val (ks, vs) = builder.unzip(res)
      vs.toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      res.toArray shouldBe col.toArray.toIterable.mapReduce(m)(plus).toArray
    }
  }

  property("Coll.groupBy") {
    def key(x: Int) = math.abs(x) % 10
    forAll(collGen) { col =>
      val res = col.groupBy(key)
      val (ks, vs) = builder.unzip(res)
      vs.flatten.toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      val pairs = col.map(x => (key(x), x))
      val res2 = pairs.groupByKey
      val (ks2, vs2) = builder.unzip(res)
      ks shouldBe ks2
      vs shouldBe vs2
    }
  }

  property("Coll.reverse") {
    forAll(collGen) { col =>
      val res = col.reverse
      res.toArray shouldBe col.toArray.reverse
      val pairs = col.zip(col)
      pairs.reverse.toArray shouldBe pairs.toArray.reverse
    }

    forAll(superGen) {
      case cl: PairColl[_, _] => {
        val res = cl.reverse
        res.toArray shouldBe cl.toArray.reverse
        val pairs = cl.zip(cl)
        pairs.reverse.toArray shouldBe pairs.toArray.reverse
      }
      case _ => false shouldBe true
    }
  }

  property("Coll.take") {
    forAll(collGen) { col =>
      val n = col.length / 2
      val res = col.take(n)
      res.toArray shouldBe col.toArray.take(n)
      val pairs = col.zip(col)
      pairs.take(n).toArray shouldBe pairs.toArray.take(n)
    }

    forAll(superGen) {
      case col: PairColl[_, _] => {
        val n = col.length / 2
        val res = col.take(n)
        res.toArray shouldBe col.toArray.take(n)
        val pairs = col.zip(col)
        pairs.take(n).toArray shouldBe pairs.toArray.take(n)
      }
      case _ => false shouldBe true
    }
  }

  property("Coll.distinct") {
    forAll(collGen) { col =>
      val res = col.distinct
      res.toArray shouldBe col.toArray.distinct
      val pairs = col.zip(col)
      pairs.distinct.toArray shouldBe pairs.toArray.distinct
    }
    forAll(getSuperGen(2, Gen.oneOf(getCollReplGen(choose(1, 10), valGen),
      getCollReplGen(choose(1, 10), valGen)))) {
      case col: PairColl[_, _] => {
        val res = col.distinct
        res.toArray shouldBe col.toArray.distinct
        val pairs = col.zip(col)
        pairs.distinct.toArray shouldBe pairs.toArray.distinct
      }
      case _ => false shouldBe true
    }
  }

  property("Coll.equals") {
    forAll(valGen, indexGen) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

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
  }

  property("PairColl.mapFirst") {
    forAll(collGen) { col =>
      val pairs = col.zip(col)
      pairs.mapFirst(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (inc(x), y) }
      pairs.mapSecond(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (x, inc(y)) }
    }
  }

  property("Coll.unionSet") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.unionSet(col2)
      res.toArray shouldBe (col1.toArray.union(col2.toArray).distinct)
    }
    builder.replicate(2, 10).unionSet(builder.replicate(3, 10)).toArray shouldBe Array(10)
  }

  property("Coll.diff") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.diff(col2)
      res.toArray shouldBe (col1.toArray.diff(col2.toArray))
    }
    builder.replicate(2, 10).diff(builder.replicate(1, 10)).toArray shouldBe Array(10)
  }

  property("Coll.intersect") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.intersect(col2)
      res.toArray shouldBe (col1.toArray.intersect(col2.toArray))
    }
    builder.replicate(2, 10).intersect(builder.replicate(3, 10)).toArray shouldBe Array(10, 10)
  }

  property("CollBuilder.xor") {
    forAll(bytesGen, bytesGen) { (col1, col2) =>
      val n = col1.length min col2.length
      val c1 = col1.take(n)
      val c2 = col2.take(n)
      builder.xor(c1, c2).toArray shouldBe c1.toArray.zip(c2.toArray).map { case (l,r) => (l ^ r).toByte }
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
//    test(builder.fromItems(0))
//    val gen = containerOfN[Array, Int](100, choose(20, 100))
//        .map(xs => builder.fromArray(xs.distinct))
    forAll(collGen) { col =>
      test(col)
    }
  }

  property("CViewColl.correctWork") {
    forAll(collGen) { coll =>
      val view = builder.makeView(coll, complexFunction)
      val usual = coll.map(complexFunction)
      view.toArray shouldBe usual.toArray
    }
  }
}
