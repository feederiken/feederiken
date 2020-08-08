package feederiken.actors

import zio._, zio.actors._

import feederiken.Env

object Collector {
  sealed trait State extends Product with Serializable
  private case class Ready(dispatcher: Dispatcher, saver: Saver) extends State
  private case class Busy(dispatcher: Dispatcher, saver: Saver, job: Job)
      extends State
  def initial(dispatcher: Dispatcher, saver: Saver): State =
    Ready(dispatcher, saver)

  sealed trait Commands[+_] extends Product with Serializable
  case class Start(job: Job) extends Commands[Unit]
  case class Process(result: Job.Result) extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      state match {
        case Ready(dispatcher, saver) =>
          msg match {
            case Start(job) =>
              for {
                self <- context.self[Commands]
                job <- UIO(job.copy(collector = self))
                _ <- dispatcher ! Dispatcher.Start(job)
              } yield (Busy(dispatcher, saver, job), ())
            case Process(_) =>
              IO.succeed(state, ())
          }
        case Busy(dispatcher, saver, job) =>
          msg match {
            case Start(_) =>
              IO.fail(new IllegalStateException("cannot start a job while another is already in progress"))
            case Process(result) =>
              val currentScore = job.score(result)
              def maybeSave = IO.when(currentScore >= job.minScore) {
                  saver ! Saver.Save(result)
              }
              def updateJob =
                if (currentScore >= job.maxScore)
                  { dispatcher ! Dispatcher.Reset } as Ready(dispatcher, saver)
                else if (currentScore >= job.minScore) {
                    for {
                    job <- UIO(job.copy(minScore = currentScore + 1))
                  _ <- dispatcher ! Dispatcher.Start(job)
                     } yield Busy(dispatcher, saver, job)
                } else IO.succeed(state)
                for {
                    _ <- maybeSave
                    state <- updateJob
                } yield (state, ())
          }
      }
  }
}