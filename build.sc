import mill._, scalalib._, scalafmt._

object app extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.2"

  val zioVersion = "1.0.0"
  val zioActorsVersion = "0.0.7"
  val zioLoggingVersion = "0.4.0"

  def ivyDeps = Agg(
    ivy"com.monovore::decline:1.2.0",
    ivy"dev.zio::zio:$zioVersion",
    ivy"dev.zio::zio-streams:$zioVersion",
    ivy"dev.zio::zio-logging:$zioLoggingVersion",
    ivy"dev.zio::zio-actors:$zioActorsVersion",
    ivy"org.bouncycastle:bcpg-jdk15on:1.65",
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalacheck::scalacheck:1.14.3",
    )
    def testFrameworks = Seq("org.scalacheck.ScalaCheckFramework")
  }
}
