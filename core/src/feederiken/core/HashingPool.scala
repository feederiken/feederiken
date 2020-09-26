package feederiken.core

import zio._, zio.internal.Executor
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/** A fast, work-stealing executor to run hashing operations on.
 */
object HashingPool {
  trait Service {
    def executor: Executor
  }

  final class FromExecutor(val executor: Executor) extends Service

  private def workStealingPool(parallelismLevel: Int) = ZManaged.make {
    UIO(Executors.newWorkStealingPool(parallelismLevel))
  } { pool =>
    UIO(pool.shutdown)
  }

  def live: URLayer[Has[SearchParameters], HashingPool] = ZLayer.fromManaged {
    for {
      tc <- ZManaged.fromEffect(SearchParameters.threadCount)
      es <- workStealingPool(tc)
      ec = ExecutionContext.fromExecutorService(es)
      executor = Executor.fromExecutionContext(Int.MaxValue)(ec)
    } yield new FromExecutor(executor)
  }

  def any: URLayer[HashingPool, HashingPool] = ZLayer.service

  def execute[R, E, A](zio: ZIO[R, E, A]): ZIO[R with HashingPool, E, A] =
    ZIO.service[Service] >>= { s => zio.lock(s.executor) }
}
