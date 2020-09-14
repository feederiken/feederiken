import mill._, scalalib._, scalafmt._

object Deps {
  val bouncycastleVersion = "1.65"
  val catsVersion = "2.2.0"
  val declineVersion = "1.3.0"
  val scalacheckVersion = "1.14.3"
  val slf4jVersion = "1.7.28"
  val zioVersion = "1.0.1"
  val zioActorsVersion = "0.0.7"
  val zioLoggingVersion = "0.4.0"

  val zio = ivy"dev.zio::zio:$zioVersion"
  val zioStream = ivy"dev.zio::zio-streams:$zioVersion"
  val zioActors = ivy"dev.zio::zio-actors:$zioActorsVersion"
  val zioLogging = ivy"dev.zio::zio-logging:$zioLoggingVersion"
  val zioLoggingSlf4j = ivy"dev.zio::zio-logging-slf4j:$zioLoggingVersion"
  val zoclCore = ivy"io.github.feederiken::zocl-core:0.0.1"
  val bcpg = ivy"org.bouncycastle:bcpg-jdk15on:1.65"
  val catsCore = ivy"org.typelevel::cats-core:$catsVersion"
  val decline = ivy"com.monovore::decline:$declineVersion"
  val scalacheck = ivy"org.scalacheck::scalacheck:$scalacheckVersion"
  val slf4jSimple = ivy"org.slf4j:slf4j-simple:$slf4jVersion"
}

import Deps._

trait common extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.2"
}

object core extends common {
  def ivyDeps = Agg(zio, zioStream, zioLogging, bcpg, zoclCore)

  object test extends Tests {
    def ivyDeps = Agg(scalacheck)
    def testFrameworks = Seq("org.scalacheck.ScalaCheckFramework")
  }
}

object messages extends common {
  def moduleDeps = Seq(core)

  def ivyDeps = Agg(zio, zioActors)
}

object app extends common {
  def moduleDeps = Seq(core, messages)

  def ivyDeps = Agg(catsCore, decline, zio, zioLoggingSlf4j, slf4jSimple)
}
