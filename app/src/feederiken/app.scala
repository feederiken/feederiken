package feederiken

import zio._
import zio.stream._
import zio.logging._
import java.util.Date

import pgp._
import java.io.FileOutputStream
import feederiken.actors.FeederikenSystem

object FeederikenApp extends App {
  val UserId = "Anonymous"
  val now = UIO(new Date)
  val availableProcessors = for {
    n <- UIO(java.lang.Runtime.getRuntime.availableProcessors)
    _ <- log.info(s"Detected $n parallel threads")
  } yield n

  def genCandidates(creationTime: Date) = {
    val creationTimeRange = Chunk.fromIterable(for {
      t <- 0 until 1024
    } yield Date.from(creationTime.toInstant().minusSeconds(t)))
    ZStream[PGP, Nothing, DatedKeyPair] {
      for {
        kpg <- keyPairGenerator
      } yield for {
        kp <- genKeyPair(kpg)
        batch <- creationTimeRange.mapM(dateKeyPair(kp)(_))
      } yield batch
    }
  }

  def performSearch(prefix: Seq[Byte]) =
    for {
      creationTime <- now
      stream = genCandidates(creationTime).filter {
        _.getPublicKey.getFingerprint.startsWith(prefix)
      }
      result <- stream.take(1).runHead.someOrFailException
      _ <- log.info("Found matching keypair")
    } yield result

  def printRing(ring: KeyRing) =
    log.info("Saving results to results.asc") *>
      Managed
        .fromAutoCloseable(IO(new FileOutputStream("results.asc", true)))
        .use(saveRing(ring, _))

  def measureFreq[R, E](
      action: ZIO[R, E, Any]
  ): ZIO[R with clock.Clock, E, Double] =
    for {
      r <- action.timed
      t = r._1
      freq = 1e9 / t.toNanos
    } yield freq

  def interpret(command: Command): RIO[Env, Unit] =
    command match {
      case command: Bench =>
        for {
          threadCount <- command.j.fold(availableProcessors.map(2*_))(UIO(_))
          n = command.n
          creationTime <- now
          _ <- log.info(
            s"Benchmarking $n iterations ${if (threadCount == 1) "without parallelism"
            else s"$threadCount times concurrently"}"
          )
          workers <- ZIO.forkAll(
            Iterable.fill(threadCount) {
              measureFreq(genCandidates(creationTime).take(n).runDrain)
            }
          )
          freqs <- workers.join
          freq = n * freqs.sum
          _ <- console.putStrLn(
            s"${if (threadCount == 1) "Single-threaded" else "Parallel"} hashrate: $freq Hz"
          )
        } yield ()

      case command: Search =>
        for {
          threadCount <- command.j.fold(availableProcessors.map(2*_))(UIO(_))
          creationTime <- now

          // bruteforce in parallel
          search = performSearch(command.prefix.toVector)
          result <- Iterable.fill(threadCount)(search).reduce(_ raceFirst _)

          // append result to results.asc
          ring <- makeRing(result, UserId)
          _ <- printRing(ring)
        } yield ()

      case command: Node =>
        for {
          sys <- FeederikenSystem.start("node", Some(command.configFile.toFile))
          path <- sys.worker.path
          _ <- console.putStrLn(s"Path is: $path")
          _ <- IO.never // Let actors work until interrupted
        } yield ()

      case command: Coordinator =>
        for {
          sys <- FeederikenSystem.start("coordinator")
          remoteWorkers <- ZIO.foreach(command.nodes.toList)(sys.select(_))
          workers =
            Option.when(command.localNode)(sys.worker).toList ++ remoteWorkers
          prefix = command.prefix.toList

          exit <- sys.executeOn(workers) {
            for {
              result <- performSearch(prefix)
              ring <- makeRing(result, UserId)
            } yield ring.getEncoded // The full object wont serialize
          }
          encodedResult <- ZIO.done(exit)
          buf = new java.io.ByteArrayInputStream(encodedResult)
          ring <- loadRing(buf)
          _ <- printRing(ring)
        } yield ()
    }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      env <- system.envs.orElseSucceed(Map.empty)
      code <- CLI.top.parse(args, env) match {
        case Left(h) =>
          console.putStrLn(h.toString).as {
            if (h.errors.isEmpty) ExitCode.success
            else ExitCode.failure
          }
        case Right(command) =>
          interpret(command)
            .provideCustomLayer(PGP.bouncyCastle ++ Logging.console())
            .exitCode
      }
    } yield code
}
