package feederiken.actors

import zio._, zio.actors._

import feederiken.core._, feederiken.messages._

object FeederikenWorker {
  import Worker._

  sealed trait State extends Product with Serializable
  private case class Ready(finished: Promise[Nothing, Unit]) extends State
  private case class Busy(
      fiber: Fiber[Nothing, Unit],
      finished: Promise[Nothing, Unit],
  ) extends State
  def initial(finished: Promise[Nothing, Unit]): State = Ready(finished)

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    private def performWork(job: Job) =
      performSearch(job.goal, job.mode, job.minScore) foreach { kp =>
        job.collector ! Collector.Process(kp)
      }

    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      state match {
        case Ready(finished) =>
          msg match {
            case Reset => IO.succeed(Ready(finished), ())
            case Start(job) =>
              for {
                fiber <- performWork(job).orDie.fork
              } yield (Busy(fiber, finished), ())
          }
        case Busy(fiber, finished) =>
          msg match {
            case Reset =>
              for {
                _ <- fiber.interrupt
                _ <- finished.succeed()
              } yield (Ready(finished), ())
            case Start(job) =>
              for {
                _ <- fiber.interrupt
                fiber <- performWork(job).orDie.fork
              } yield (Busy(fiber, finished), ())
          }
      }
  }
}
