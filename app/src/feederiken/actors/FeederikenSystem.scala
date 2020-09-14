package feederiken.actors

import zio._, zio.actors._, zio.duration._, zio.logging._
import java.io.File

import feederiken.core._, feederiken.messages._

final class FeederikenSystem(
    protected val as: ActorSystem,
    val dispatcher: Dispatcher.Ref,
) {
  def select[F[+_]](path: String): Task[ActorRef[Any]] = as.select(path)

  def attachTo(dispatcher: Dispatcher.Ref): RIO[Env, Unit] =
    for {
      finished <- Promise.make[Nothing, Unit]
      sup = actors.Supervisor.none
      worker <-
        as.make("worker", sup, FeederikenWorker.initial(finished), FeederikenWorker.Interpreter)
      _ <- dispatcher ? Dispatcher.Attach(worker)
      _ <- finished.await
    } yield ()

  def search(
      goal: Chunk[Byte],
      mode: Mode,
      minScore: Int,
      maxScore: Int,
  ): RIO[Env, Unit] =
    for {
      finished <- Promise.make[Nothing, Unit]
      sup = actors.Supervisor.none
      saver <- as.make("saver", sup, FeederikenSaver.initial, FeederikenSaver.Interpreter)
      collector <- as.make(
        "collector",
        sup,
        FeederikenCollector.initial(dispatcher, saver, finished),
        FeederikenCollector.Interpreter,
      )
      _ <- collector ? Collector.Start(
        Job(goal, mode, minScore, maxScore, collector)
      )
      _ <- finished.await
    } yield ()
}
object FeederikenSystem {

  def start(name: String, configFile: Option[File]) =
    for {
      as <- ActorSystem(name, configFile)
      sup = actors.Supervisor.none
      dispatcher <-
        as.make("dispatcher", sup, TrivialDispatcher.initial, TrivialDispatcher.Interpreter)
    } yield new FeederikenSystem(as, dispatcher)
}
