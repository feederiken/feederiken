// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

import zio._
import zio.stream._
import zio.logging._
import com.monovore.decline._
import cats.syntax.all._
import cats.instances.list._
import cats.data.Chain
import java.util.Date

import pgp._
import java.io.FileOutputStream


object Feederiken extends App {
  type Env = ZEnv with PGP with Logging

  def genCandidates(creationTime: Date) = {
    val creationTimeRange = Chunk.fromIterable(0 until 64 map { creationTime.toInstant().plusSeconds(_) } map { Date.from })
    ZStream[PGP, Nothing, DatedKeyPair] {
      for {
        kpg <- keyPairGenerator
      } yield
      for {
        kp <- genKeyPair(kpg)
        batch <- creationTimeRange.mapM(dateKeyPair(kp)(_))
      } yield batch
    }
  }

  def bruteForceKey(prefix: Array[Byte], creationTime: Date) =
    genCandidates(creationTime).filter(_.getPublicKey.getFingerprint.startsWith(prefix)).take(1).runHead.someOrFailException <* log.info("Found matching keypair")

  def measureFreq[R, E](action: ZIO[R, E, Any]): ZIO[R with clock.Clock, E, Double] = for {
    r <- action.timed
    t = r._1
    freq = 1e9 / t.toNanos
  } yield freq

  val now = IO(new Date)
  val availableProcessors = IO(java.lang.Runtime.getRuntime.availableProcessors).tap(n => log.info(s"Detected $n parallel threads"))

  def bench = Command[RIO[Env, Unit]]("bench", "benchmark CPU hashrate") {
    (
      Opts.option[Int]("n", "how many keys to generate for benchmarking").withDefault(10000).validate("n must be positive")(_ > 0),
      Opts.flag("parallel", "run a parallel benchmark").orFalse,
      Opts.option[Int]("j", "# of concurrent threads to use").orNone,
    ).mapN { (n, parallel, j) =>
      for {
        threadCount <- ZIO.getOrFail(j).orElse(if (parallel) availableProcessors else UIO(1))
        creationTime <- now
        _ <- log.info(s"Benchmarking $n iterations ${if (threadCount == 1) "without parallelism" else s"$threadCount times concurrently"}")
        workers <- ZIO.forkAll(Iterable.fill(threadCount)(measureFreq(genCandidates(creationTime).take(n).runDrain)))
        freqs <- workers.join
        freq = n * freqs.sum
        _ <- console.putStrLn(s"${if (threadCount == 1) "Single-threaded" else "Parallel"} hashrate: $freq Hz")
      } yield ()
    }
  }

  def search = Command[RIO[Env, Unit]]("search", "search for a vanity key") {
    for {
      prefix <- Opts.option[String]("prefix", "key prefix to look for (hex)").withDefault("feed").mapValidated {
        _.filterNot(_.isWhitespace).grouped(2).toList.foldMap { s =>
          try Chain(Integer.parseUnsignedInt(s, 16).toByte).valid
          catch {
            case _: NumberFormatException => s"Invalid byte: $s".invalidNel
          }
        }
      }
    } yield
    for {
      threadCount <- availableProcessors
      creationTime <- now

      // bruteforce in parallel
      result <- Iterable.fill(threadCount)(bruteForceKey(prefix.iterator.toArray, creationTime)).reduce(_ raceFirst _)

      // append result to results.asc
      resultRing <- makeRing(result, "anonymous")
      _ <- log.info("Saving results to results.asc")
      _ <- Managed.fromAutoCloseable(IO(new FileOutputStream("results.asc", true))).use(saveRing(resultRing, _))
    } yield ()
  }
  def top = Command("feederiken", "vanity PGP key generator") {
    Opts.subcommands(search, bench)
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    top.parse(args) match {
      case Left(h) =>
        console.putStrLn(h.toString).as {
          if (h.errors.isEmpty) ExitCode.success
          else ExitCode.failure
        }
      case Right(program) =>
        program.provideCustomLayer(PGP.bouncyCastle ++ Logging.console()).exitCode
    }
}
