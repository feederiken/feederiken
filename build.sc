import mill._, scalalib._, scalafmt._

object versions {
  val bouncycastleVersion = "1.65"
  val catsVersion = "2.2.0"
  val declineVersion = "1.3.0"
  val scalacheckVersion = "1.14.3"
  val slf4jVersion = "1.7.28"
  val zioVersion = "1.0.1"
  val zioActorsVersion = "0.0.7"
  val zioLoggingVersion = "0.4.0"
}

import versions._

trait common extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.2"
}

object core extends common {
  def ivyDeps = Agg(
    ivy"dev.zio::zio:$zioVersion",
    ivy"dev.zio::zio-streams:$zioVersion",
    ivy"dev.zio::zio-logging:$zioLoggingVersion",
    ivy"org.bouncycastle:bcpg-jdk15on:$bouncycastleVersion",
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalacheck::scalacheck:$scalacheckVersion",
    )
    def testFrameworks = Seq("org.scalacheck.ScalaCheckFramework")
  }
}

object messages extends common {
  def moduleDeps = Seq(core)

  def ivyDeps = Agg(
    ivy"dev.zio::zio:$zioVersion",
    ivy"dev.zio::zio-actors:$zioActorsVersion",
  )
}

object app extends common {
  def moduleDeps = Seq(core, messages)

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:$catsVersion",
    ivy"com.monovore::decline:$declineVersion",
    ivy"dev.zio::zio:$zioVersion",
    ivy"dev.zio::zio-logging-slf4j:$zioLoggingVersion",
    ivy"dev.zio::zio-actors:$zioActorsVersion",
    ivy"org.slf4j:slf4j-simple:$slf4jVersion",
  )
}
