package object feederiken {

  import actors.FeederikenSystem
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

  def genCandidates(
      creationTime: Date
  ): ZStream[Env, Throwable, DatedKeyPair] = {
    val creationTimeRange = Chunk.fromIterable(for {
      t <- 0 until 1024
    } yield Date.from(creationTime.toInstant().minusSeconds(t)))
    ZStream {
      for {
        kpg <- keyPairGenerator
      } yield for {
        kp <- genKeyPair(kpg)
        batch <- creationTimeRange.mapM(dateKeyPair(kp, _))
      } yield batch
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
        stream = parallelize {
          genCandidates(creationTime).filter { kp =>
            mode.score(goal, kp.fingerprint) >= minScore
          }
        }
        result <- stream.take(1).runHead.someOrFailException
        currentScore = mode.score(goal, result.fingerprint)
        _ <- log.info(s"Found matching key (score=$currentScore)")
        _ <- saveResult(result)
        r <-
          if (currentScore < maxScore)
            rec(currentScore + 1)
          else
            IO.unit
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

  def interpret(command: Command): RIO[ZEnv with PGP with Logging, Unit] =
    (command match {
      case command: Bench =>
        for {
          threadCount <- SearchParameters.threadCount
          n = command.n
          creationTime <- now
          _ <- log.info(
            s"Benchmarking $n iterations ${if (threadCount == 1) "without parallelism"
            else s"$threadCount times concurrently"}"
          )
          stream = parallelize(genCandidates(creationTime))
          nfreq <- measureFreq(stream.take(n).runDrain)
          freq = n * nfreq
          _ <- console.putStrLn(
            s"${if (threadCount == 1) "Single-threaded" else "Parallel"} hashrate: $freq Hz"
          )
        } yield ()

      case command: Search =>
        for {
          threadCount <- SearchParameters.threadCount
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
    }).provideSomeLayer(SearchParameters.fromCommand(command))
}
