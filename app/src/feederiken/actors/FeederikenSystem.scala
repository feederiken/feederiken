package feederiken.actors

import zio._, zio.actors._, zio.duration._, zio.logging._
import java.io.File

import feederiken.{Env, Mode}

final class FeederikenSystem(
    protected val as: ActorSystem,
    val dispatcher: Dispatcher,
) {
  def select[F[+_]](path: String): Task[ActorRef[Any]] = as.select(path)

  def attachTo(dispatcher: Dispatcher): RIO[Env, Unit] =
    for {
      finished <- Promise.make[Nothing, Unit]
      sup = actors.Supervisor.none
      worker <- as.make("worker", sup, Worker.initial(finished), Worker.Interpreter)
      _ <- dispatcher ! Dispatcher.Attach(worker)
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
      saver <- as.make("saver", sup, Saver.initial, Saver.Interpreter)
      collector <- as.make(
        "collector",
        sup,
        Collector.initial(dispatcher, saver, finished),
        Collector.Interpreter,
      )
    _ <- collector ! Collector.Start(Job(goal, mode, minScore, maxScore, collector))
    _ <- finished.await
  } yield ()
}
object FeederikenSystem {
  def start(name: String, configFile: Option[File]): RManaged[Env, FeederikenSystem] = {
    val acquire: RIO[Env, FeederikenSystem] =
      for {
        as <- ActorSystem(name, configFile)
        sup = actors.Supervisor.none
        dispatcher <-
          as.make("dispatcher", sup, Dispatcher.initial, Dispatcher.Interpreter)
      } yield new FeederikenSystem(as, dispatcher)
    ZManaged.make(acquire)(_.as.shutdown.orDie)
  }
}
