package object feederiken {

  import actors._
  import pgp._

  import zio._, zio.stream._, zio.logging._

  import java.io.FileOutputStream, java.util.Date

  val UserId = "Anonymous"

  type Services = ZEnv with PGP with Logging
  type Env = Services with Has[SearchParameters]

  val now: URIO[Services, Date] = UIO(new Date)
  val availableProcessors: URIO[Services, Int] = for {
    n <- UIO(java.lang.Runtime.getRuntime.availableProcessors)
    _ <- log.info(s"Detected $n parallel threads")
  } yield n

  def genCandidates: ZStream[Env, Throwable, DatedKeyPair] = {
    ZStream.unwrap {
      for {
        creationTime <- now
        creationTimeRange = Chunk.fromIterable(for {
          t <- 0 until 1024
        } yield Date.from(creationTime.toInstant().minusSeconds(t)))
      } yield ZStream {
        for {
          kpg <- keyPairGenerator
        } yield for {
          kp <- genKeyPair(kpg)
          batch <- creationTimeRange.mapM(dateKeyPair(kp, _))
        } yield batch
      }
    }
  }

  def parallelize[R, E, O](
      stream: ZStream[R, E, O]
  ): ZStream[R with Has[SearchParameters], E, O] =
    ZStream.unwrap {
      for {
        threadCount <- SearchParameters.threadCount
      } yield ZStream.mergeAllUnbounded()(Seq.fill(threadCount)(stream): _*)
    }

  def saveResult(result: DatedKeyPair) =
    for {
      _ <- log.info(s"Dumping private key ${result.fingerprint}")
      ring <- makeRing(result, UserId)
      out <- UIO(Console.out)
      _ <- saveRing(ring, out)
    } yield ()

  def measureFreq[R, E](
      action: ZIO[R, E, Any]
  ): ZIO[R with clock.Clock, E, Double] =
    for {
      r <- action.timed
      t = r._1
      freq = 1e9 / t.toNanos
    } yield freq

  def interpret(command: Command): RIO[ZEnv with PGP with Logging, Unit] =
    (command match {
      case command: Bench =>
        for {
          threadCount <- SearchParameters.threadCount
          n = command.n
          _ <- log.info(
            s"Benchmarking $n iterations ${if (threadCount == 1) "without parallelism"
            else s"$threadCount times concurrently"}"
          )
          stream = parallelize(genCandidates)
          nfreq <- measureFreq(stream.take(n).runDrain)
          freq = n * nfreq
          _ <- console.putStrLn(
            s"${if (threadCount == 1) "Single-threaded" else "Parallel"} hashrate: $freq Hz"
          )
        } yield ()

      case command: Search =>
        for {
          sys <- FeederikenSystem.start(
            "coordinator",
            command.configFile.map(_.toFile),
          )

          goal = command.goal.iterator.to(ChunkLike)
          mode = command.mode
          maxScore = command.maxScore.foldRight(
            mode.maxScore(goal, FingerprintLength)
          )(_ min _)
          minScore = command.minScore.foldRight(maxScore)(_ min _)
          _ <- IO.when(command.localSearch) {
            sys.attachTo(sys.dispatcher)
          }
          _ <- sys.search(goal, mode, minScore, maxScore)
          _ <- IO.never // Let actors work until interrupted
        } yield ()

      case command: Node =>
        for {
          sys <- FeederikenSystem.start(
            command.nodeName,
            Some(command.configFile.toFile),
          )
          dispatcher <- sys.select(command.dispatcherPath)
          _ <- sys.attachTo(dispatcher)
          _ <- IO.never // Let actors work until interrupted
        } yield ()
    }).provideSomeLayer(SearchParameters.fromCommand(command))
}
