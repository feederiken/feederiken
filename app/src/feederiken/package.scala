import feederiken.actors._, feederiken.core._, feederiken.pgp._

import zio._, zio.logging._

package object feederiken {
  private def searchParameters(command: Command): URLayer[Logging, Has[SearchParameters]] =
    ZLayer.fromEffect {
      for {
        threadCount <- command.j.fold(availableProcessors)(UIO(_))
      } yield SearchParameters(threadCount)
    }

  private def interpret1(command: Command) = command match {
    case command: Bench =>
      for {
        threadCount <- SearchParameters.threadCount
        n = command.n
        _ <- log.info(
          s"Benchmarking $n iterations ${if (threadCount == 1) "without parallelism"
          else s"over $threadCount threads"}"
        )
        stream = parallelize(genCandidates)
        freq <- stream.run(measureFreq(n))
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
        _ <- RIO.when(command.localSearch) {
          sys.attachTo(sys.dispatcher).forkAs("worker")
        }
        path <- sys.dispatcher.path
        _ <- RIO.when(command.configFile.nonEmpty) {
          console.putStrLnErr(s"dispatcher address: $path")
        }
        _ <- sys.search(goal, mode, minScore, maxScore)
      } yield ()

    case command: Node =>
      for {
        sys <- FeederikenSystem.start(
          command.nodeName,
          Some(command.configFile.toFile),
        )
        dispatcher <- sys.select(command.dispatcherPath)
        _ <- sys.attachTo(dispatcher)
      } yield ()
  }

  def interpret(command: Command): RIO[ZEnv with PGP with Logging, Unit] =
    interpret1(command).provideSomeLayer[ZEnv with PGP with Logging](searchParameters(command))
}
