import mill._, scalalib._, scalafmt._

object app extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.2"

  def ivyDeps = Agg(
    ivy"com.monovore::decline:1.2.0",
    ivy"dev.zio::zio:1.0.0-RC20",
    ivy"dev.zio::zio-streams:1.0.0-RC20",
    ivy"dev.zio::zio-logging:0.3.2",
    ivy"org.bouncycastle:bcpg-jdk15on:1.65",
  )
}
