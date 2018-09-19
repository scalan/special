package wrappers.scalan.meta {
  import scalan._

  import impl._

  import special.wrappers.WrappersModule

  trait WRTypes extends Base { self: WrappersModule =>
    import WRType._;
    @External("RType") @Liftable trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      @External def name: Rep[String]
    };
    trait WRTypeCompanion
  }
}