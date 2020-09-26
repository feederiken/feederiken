package feederiken.core

import zio._, stream._
import zio.internal.Executor
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/** A lean thread pool to run CPU hashing operations on.
  */
object HashingPool {
  trait Service {
    def executors: Chunk[Executor]
  }

  final class FromExecutors(val executors: Chunk[Executor]) extends Service

  private val makeSingleThreadExecutor = ZManaged.make {
    UIO(Executors.newSingleThreadExecutor())
  } { pool =>
    UIO(pool.shutdown)
  }

  def live: URLayer[Has[SearchParameters], HashingPool] =
    ZLayer.fromManaged {
      for {
        tc <- ZManaged.fromEffect(SearchParameters.threadCount)
        executors <- ZManaged.foreach(Chunk.fill(tc)()) { _ =>
          for {
            es <- makeSingleThreadExecutor
            ec = ExecutionContext.fromExecutorService(es)
            executor = Executor.fromExecutionContext(Int.MaxValue)(ec)
          } yield executor
        }
      } yield new FromExecutors(executors)
    }

  def any: URLayer[HashingPool, HashingPool] = ZLayer.service

  private def executors = ZIO.service[Service].map { _.executors }

  def parallelize[R, E, O](
      stream: ZStream[R, E, O]
  ): ZStream[R with HashingPool, E, O] =
    ZStream.unwrap {
      for {
        executors <- executors
      } yield ZStream.mergeAllUnbounded()(
        executors.map(e => ZStream(process = stream.process.map(_.lock(e)))): _*
      )
    }
}
