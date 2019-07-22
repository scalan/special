package scalan

import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, Compiler}
import scalan.util.FileUtil

trait TestContextsEx extends TestContexts {

  // TODO change API to use defaultCompilers here! See JNI_MsfItTests and others
  abstract class TestCompilerContext(testName: String) {
    def this() = this(currentTestNameAsFileName)

    val compiler: Compiler[_ <: Scalan]
    import compiler._

    def test[A,B](functionName: String, f: => scalan.Rep[A => B]): CompilerOutput[A, B] = {
      buildExecutable(FileUtil.file(prefix + "/" + testName, functionName), functionName, f, GraphVizConfig.default)(defaultCompilerConfig)
    }
    def test[A,B](f: => scalan.Rep[A => B]): CompilerOutput[A, B] = test(testName, f)

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => scalan.Sym)*): Unit = stage(scalan)(testName, name, sfs)
    def emit(name: String, s1: => scalan.Sym): Unit = emitF(name, () => s1)
    def emit(name: String, s1: => scalan.Sym, s2: => scalan.Sym): Unit =
      emitF(name, () => s1, () => s2)
    def emit(name: String, s1: => scalan.Sym, s2: => scalan.Sym, s3: => scalan.Sym): Unit =
      emitF(name, () => s1, () => s2, () => s3)
    def emit(s1: => scalan.Sym): Unit = emitF(testName, () => s1)
    def emit(s1: => scalan.Sym, s2: => scalan.Sym): Unit =
      emitF(testName, () => s1, () => s2)
    def emit(s1: => scalan.Sym, s2: => scalan.Sym, s3: => scalan.Sym): Unit =
      emitF(testName, () => s1, () => s2, () => s3)
  }

  abstract class TestContextEx(val testName: String) extends ScalanEx with TestContextApi {
    def this() = this(currentTestNameAsFileName)

    override val invokeAll = true
    override def isInvokeEnabled(d: Def[_], m: Method) = invokeAll
    override def shouldUnpack(e: Elem[_]) = true

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => Sym)*): Unit = stage(this)(testName, name, sfs)
  }
}

abstract class BaseCtxTestsEx extends BaseTests with TestContextsEx

