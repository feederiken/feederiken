package feederiken.actors

import zio._, zio.actors._, zio.duration._, zio.logging._
import java.io.File

import feederiken.Mode

final class FeederikenSystem(
    protected val as: ActorSystem,
    val collector: Collector,
    val dispatcher: Dispatcher,
    val worker: Worker,
) {
  def select[F[+_]](path: String): Task[ActorRef[Any]] = as.select(path)

  def attachTo(dispatcher: Dispatcher): Task[Unit] =
    dispatcher ! Dispatcher.Attach(worker)

  def search(
      goal: Chunk[Byte],
      mode: Mode,
      minScore: Int,
      maxScore: Int,
  ): Task[Unit] =
    collector ! Collector.Start(Job(goal, mode, minScore, maxScore, collector))
}
object FeederikenSystem {

  def start(name: String, configFile: Option[File]) =
    for {
      as <- ActorSystem(name, configFile)
      sup = actors.Supervisor.none
      dispatcher <-
        as.make("dispatcher", sup, Dispatcher.initial, Dispatcher.Interpreter)
      saver <- as.make("saver", sup, Saver.initial, Saver.Interpreter)
      collector <- as.make(
        "collector",
        sup,
        Collector.initial(dispatcher, saver),
        Collector.Interpreter,
      )
      worker <- as.make("worker", sup, Worker.initial, Worker.Interpreter)
    } yield new FeederikenSystem(as, collector, dispatcher, worker)
}
