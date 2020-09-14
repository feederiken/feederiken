package feederiken.actors

import zio._, zio.actors._

import feederiken.core._, feederiken.messages._

object FeederikenCollector {
  import Collector._

  sealed trait State extends Product with Serializable
  private case class Ready(
      dispatcher: Dispatcher.Ref,
      saver: Saver.Ref,
      finished: Promise[Nothing, Unit],
  ) extends State
  private case class Busy(
      dispatcher: Dispatcher.Ref,
      saver: Saver.Ref,
      job: Job,
      finished: Promise[Nothing, Unit],
  ) extends State
  def initial(
      dispatcher: Dispatcher.Ref,
      saver: Saver.Ref,
      finished: Promise[Nothing, Unit],
  ): State =
    Ready(dispatcher, saver, finished)

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      state match {
        case Ready(dispatcher, saver, finished) =>
          msg match {
            case Start(job) =>
              for {
                self <- context.self[Commands]
                job <- UIO(job.copy(collector = self))
                _ <- dispatcher ! Dispatcher.Start(job)
              } yield (Busy(dispatcher, saver, job, finished), ())
            case Process(_) =>
              IO.succeed(state, ())
          }
        case Busy(dispatcher, saver, job, finished) =>
          msg match {
            case Start(_) =>
              IO.fail(
                new IllegalStateException(
                  "cannot start a job while another is already in progress"
                )
              )
            case Process(result) =>
              val currentScore = job.score(result)
              def maybeSave =
                IO.when(currentScore >= job.minScore) {
                  saver ? Saver.Save(result)
                }
              def updateJob =
                if (currentScore >= job.maxScore)
                  for {
                    _ <- dispatcher ! Dispatcher.Reset
                    _ <- finished.succeed()
                  } yield Ready(dispatcher, saver, finished)
                else if (currentScore >= job.minScore) {
                  for {
                    job <- UIO(job.copy(minScore = currentScore + 1))
                    _ <- dispatcher ! Dispatcher.Start(job)
                  } yield Busy(dispatcher, saver, job, finished)
                } else IO.succeed(state)
              for {
                _ <- maybeSave
                state <- updateJob
              } yield (state, ())
          }
      }
  }
}
