package feederiken

package object core {
  import feederiken.pgp._

  import zio._, zio.stream._, zio.logging._

  import java.io.FileOutputStream, java.util.Date

  val UserId = "Anonymous"

  type Env = ZEnv with PGP with Logging with Has[SearchParameters]

  val availableProcessors: URIO[Logging, Int] = for {
    n <- UIO(java.lang.Runtime.getRuntime.availableProcessors)
    _ <- log.info(s"Detected $n parallel threads")
  } yield n

  def genCandidates: ZStream[Env, Throwable, DatedKeyPair] = {
    ZStream.unwrap {
      for {
        creationTime <- clock.instant
        creationTimeRange = Chunk.fromIterable(for {
          t <- 0 until 1024
        } yield Date.from(creationTime.minusSeconds(t)))
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

  def showFingerprint(fpr: IndexedSeq[Byte]): String =
    fpr.map(_.formatted("%02X")).grouped(2).map(_.mkString).mkString(" ")

  def parallelize[R, E, O](
      stream: ZStream[R, E, O]
  ): ZStream[R with Has[SearchParameters], E, O] =
    ZStream.unwrap {
      for {
        threadCount <- SearchParameters.threadCount
      } yield ZStream.mergeAllUnbounded()(Seq.fill(threadCount)(stream): _*)
    }

  def performSearch(
    goal: Chunk[Byte],
    mode: Mode,
    minScore: Int,
    ): ZStream[Env, Throwable, DatedKeyPair] =
      parallelize {
        genCandidates.filter { kp =>
          mode.score(goal, kp.fingerprint) >= minScore
        }
      }

  def saveResult(result: DatedKeyPair) =
    for {
      _ <-
        log.info(s"Dumping private key ${showFingerprint(result.fingerprint)}")
      ring <- makeRing(result, UserId)
      out <- UIO(Console.out)
      _ <- saveRing(ring, out)
    } yield ()

  def measureFreq[I](n: Int) =
    for {
      _ <- ZSink.take[I](n / 10) // warmup
      r <- ZSink.take[I](n).timed
      t = r._2
      freq = 1e9 / t.toNanos * n
    } yield freq
}
