package scalan.it.lms

import java.io.File

import scalan.compilation.lms._
import scalan.community.ScalanCommunityExp
import scalan.compilation.lms.{LmsCompilerScala, LmsCompiler}
import scalan.it.smoke.CommunitySmokeItTests
import scalan.parrays.PArraysDslExp
import scalan.compilation.GraphVizExport
import scalan.compilation.lms.{CommunityBridge, LinalgBridge, LmsBridge, LmsCompiler}
import scalan.it.smoke.CommunitySmokeItTests
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}
import scalan.community.{ScalanCommunityDslExp, ScalanCommunityExp}
import scalan.performance.MVMs

class LmsSmokeItTests extends CommunitySmokeItTests {
  class ProgExp extends ProgCommunity with PArraysDslExp with ScalanCommunityExp with GraphVizExport with LmsCompilerCXX with VectorsDslExp with MatricesDslExp { self =>
    def makeBridge[A, B] = new LmsBridge[A, B] with CommunityBridge[A, B] with LinalgBridge[A, B] with JNIBridge[A,B] {
      val scalan = self
    }
  }
  
  override val progStaged = new ProgExp

  test("jniExtractor") {
    //FIXME: it fails on the first run and works on next runs
    val functionName = "jniExtractor"
    val dir = new File(new File("it-out", prefix), functionName)
//    progStaged.buildGraph(dir, functionName, progStaged.jniExtractor, true)(progStaged.defaultConfig)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.jniExtractor, true)(progStaged.defaultConfig)

    System.setProperty("java.library.path", new File(dir,"release").getAbsolutePath)
    println(System.getProperty("java.library.path"))
    System.loadLibrary("jniExtractor")

    val v = Array(34.0)
    val k = 77.0
     val res = (new MVMs).extractorTest( (v,k) )
//    println(res.mkString("[",",","]"))
    assert(res sameElements (v map {a => k*a}) )
  }


  test("test0simpleArith") {
    val in = 2
    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }
  test("test1simpleArrGet") {
    val in = (Array(2,3), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet, "simpleArrGet", in)
  }
  test("test2simpleMap") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.simpleMap, "simpleMap", in)
  }
  test("test3simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested, "simpleMapNested", in)
  }
  test("test4simpleZip") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.simpleZip, "simpleZip", in)
  }
  test("test5simpleZipWith") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith, "simpleZipWith", in)
  }
  test("test6simpleReduce") {
    val in = Array(2,3)
    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.simpleReduce, "simpleArith", in)
  }
  test("test7mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.mvMul, "mvMul", in)
  }
  test("test8expBaseArrays") {
    val in = Array(Array(2,3), Array(4,5))
    compareOutputWithSequential(progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays, "expBaseArrays", in)
  }
  test("test9simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.simpleIf, "simpleIf", in)
  }
  test("test10simpleSum") {
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11simpleOptionOps") {
    val in = 7
    compareOutputWithSequential(progStaged)(progSeq.simpleOptionOps, progStaged.simpleOptionOps, "simpleOptionOps", in)
  }

}
