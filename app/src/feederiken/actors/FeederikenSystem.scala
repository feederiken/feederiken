package feederiken.actors

import zio._
import zio.actors._
import zio.duration._
import java.io.File
import zio.logging.log

final class FeederikenSystem(
    protected val as: ActorSystem,
    protected val workerI: Ref[Int],
    val dispatcher: RIO[feederiken.Env, Dispatcher],
) {
  def select[F[+_]](path: String) = as.select(path)

  def execute[E, A](
      job: ZIO[feederiken.Env, E, A]
  ): RIO[feederiken.Env, Exit[E, A]] =
    for {
      dispatcher <- this.dispatcher
      p <- Promise.make[Any, Any]
      exit <- (dispatcher ? Dispatcher.Start(job, p))
        .bracket_((dispatcher ? Dispatcher.Reset).orDie) {
          p.await.run.map(_.asInstanceOf[Exit[E, A]])
        }
    } yield exit

  protected val spawnWorker: RIO[feederiken.Env, Worker] = for {
    i <- workerI.getAndUpdate(_ + 1)
    w <- as.make(
      s"worker$i",
      actors.Supervisor.none,
      Worker.initial,
      Worker.Interpreter,
    )
  } yield w

  def attachTo(dispatcher: Dispatcher, j: Int = 1) =
    (for {
      worker <- spawnWorker
      _ <- dispatcher ? Dispatcher.Attach(worker)
    } yield ()).repeat(Schedule.recurs(j - 1))
}
object FeederikenSystem {
  def start(name: String, configFile: File) =
    for {
      as <- ActorSystem(name, Some(configFile))
      sup = actors.Supervisor.none
      workerI <- Ref.make(0)
      dispatcher <-
        as.make("dispatcher", sup, Dispatcher.initial, Dispatcher.Interpreter)
          .memoize
    } yield new FeederikenSystem(as, workerI, dispatcher)
}
