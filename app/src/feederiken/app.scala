package feederiken

import zio._
import zio.stream._
import zio.logging._
import java.util.Date

import pgp._
import java.io.FileOutputStream
import feederiken.actors.{Dispatcher, FeederikenSystem}

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
        batch <- creationTimeRange.mapM(dateKeyPair(kp, _))
      } yield batch
    }
  }

  def parallelize[R, E, O](
      threadCount: Int,
      stream: ZStream[R, E, O],
  ): ZStream[R, E, O] =
    ZStream.mergeAllUnbounded()(Seq.fill(threadCount)(stream): _*)

  def performSearch(
      threadCount: Int,
      goal: Chunk[Byte],
      mode: Mode,
      minScore: Int,
      maxScore: Int,
  ) = {
    def rec(minScore: Int): RIO[Env, Unit] =
      for {
        creationTime <- now
        stream = genCandidates(creationTime).filter { kp =>
          mode.score(goal, kp.fingerprint) >= minScore
        }
        result <-
          parallelize(threadCount, stream).take(1).runHead.someOrFailException
        currentScore = mode.score(goal, result.fingerprint)
        _ <- log.info(s"Found matching key (score=$currentScore)")
        _ <- saveResult(result)
        r <- RIO.when(currentScore < maxScore) {
          rec(currentScore + 1)
        }
      } yield r // ensure tail-call
    rec(minScore)
  }

  def saveResult(result: DatedKeyPair) =
    for {
      _ <- log.info("Saving results to results.asc")
      ring <- makeRing(result, UserId)
      _ <-
        Managed
          .fromAutoCloseable(IO(new FileOutputStream("results.asc", true)))
          .use(saveRing(ring, _))
    } yield ()

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
          threadCount <- command.j.fold(availableProcessors.map(2 * _))(UIO(_))
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
          threadCount <- command.j.fold(availableProcessors.map(2 * _))(UIO(_))
          creationTime <- now

          goal = command.goal.iterator.to(ChunkLike)
          mode = command.mode
          maxScore = command.maxScore.foldRight(
            mode.maxScore(goal, FingerprintLength)
          )(_ min _)
          minScore = command.minScore.foldRight(maxScore)(_ min _)
          _ <- performSearch(threadCount, goal, mode, minScore, maxScore)
        } yield ()

      case command: Node =>
        for {
          j <- ZIO.getOrFail(command.j) orElse availableProcessors
          sys <-
            FeederikenSystem.start(command.nodeName, command.configFile.toFile)
          dispatcher <- sys.select(command.dispatcherPath)
          _ <- sys.attachTo(dispatcher, j)
          _ <- IO.never // Let actors work until interrupted
        } yield ()

      case command: Coordinator =>
        for {
          j <- ZIO.getOrFail(command.j) orElse availableProcessors
          sys <-
            FeederikenSystem.start("coordinator", command.configFile.toFile)
          dispatcher <- sys.dispatcher
          dispatcherPath <- dispatcher.path
          _ <- console.putStrLn(s"Path: $dispatcherPath")
          goal = command.goal.iterator.to(ChunkLike)
          mode = command.mode
          maxScore = command.maxScore.foldRight(
            mode.maxScore(goal, FingerprintLength)
          )(_ min _)
          minScore = command.minScore.foldRight(maxScore)(_ min _)
          _ <- RIO.when(command.localNode) {
            sys.attachTo(dispatcher, j)
          }
          _ <- log.error("coordinator not implemented") // TODO
          _ <- ZIO(???)
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
