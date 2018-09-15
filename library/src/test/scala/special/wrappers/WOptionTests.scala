package special.wrappers

import scala.language.reflectiveCalls
import scalan.Library

class WOptionTests extends WrappersTests {

  test("invokeUnlifted") {
    val ctx = new WrappersCtx
    import ctx._
    import WOption._

    val opt = Option(1)
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.get, opt.get)
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.isEmpty, opt.isEmpty)
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.isDefined, opt.isDefined)

    val none: Option[Int] = None
    val th = () => 10
    check(none, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.getOrElse(env.lifted(th)), none.getOrElse(th()))
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.getOrElse(env.lifted(th)), opt.getOrElse(th()))

    val p = (x: Int) => x == 2
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.filter(env.lifted(p)), opt.filter(p))

    val inc = (x: Int) => x + 1
    check(opt,  (env: DataEnv, xs: Rep[WOption[Int]]) => xs.fold(env.lifted(th), env.lifted(inc)), opt.fold(th())(inc))
    check(none, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.fold(env.lifted(th), env.lifted(inc)), none.fold(th())(inc))
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.map(env.lifted(inc)), opt.map(inc))

    val incOpt = (x: Int) => Option(x + 1)
    val incNone = (x: Int) => (None: Option[Int])
    check(opt, (env: DataEnv, xs: Rep[WOption[Int]]) => xs.flatMap(env.lifted(incNone)), opt.flatMap(incNone))
  }
}
