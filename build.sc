import mill._, scalalib._

object app extends ScalaModule {
  def scalaVersion = "2.13.2"

  def ivyDeps = Agg(
    ivy"com.monovore::decline:1.0.0",
    ivy"dev.zio::zio:1.0.0-RC20",
    ivy"dev.zio::zio-streams:1.0.0-RC20",
    ivy"org.bouncycastle:bcpg-jdk15on:1.65",
  )
}
