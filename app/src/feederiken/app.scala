package feederiken

import pgp._

import zio._

object FeederikenApp extends App {
  private def loggingLayer =
    logging.slf4j.Slf4jLogger.make(
      logFormat = (_, s) => s,
      rootLoggerName = Some("feederiken"),
    )

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      env <- system.envs.orElseSucceed(Map.empty)
      code <- CLI.top.parse(args, env) match {
        case Left(h) =>
          console.putStrLnErr(h.toString).as {
            if (h.errors.isEmpty) ExitCode.success
            else ExitCode.failure
          }
        case Right(command) =>
          interpret(command)
            .provideCustomLayer(PGP.bouncyCastle ++ loggingLayer)
            .exitCode
      }
    } yield code
}
