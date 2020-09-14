package feederiken.actors

import zio._, zio.actors._, zio.logging._

import feederiken.core._, feederiken.messages._

object TrivialDispatcher {
  import Dispatcher._

  sealed trait State extends Product with Serializable {
    val workers: List[Worker.Ref]
  }
  private case class Ready(workers: List[Worker.Ref]) extends State
  private case class Busy(
      workers: List[Worker.Ref],
      currentJob: Job,
  ) extends State

  val initial: State = Ready(Nil)

  type Env = Logging

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] = {
      def broadcastWorkers(msg: Worker.Commands[Any]) =
        IO.foreachPar_(state.workers) { _ ! msg }
      state match {
        case Ready(workers) =>
          msg match {
            case Attach(worker) =>
              for {
                _ <- log.info("attached 1 worker")
              } yield (Ready(worker :: workers), ())
            case Reset =>
              UIO(state, ())
            case Start(job) =>
              broadcastWorkers(Worker.Start(job)) *>
                UIO(Busy(workers, job), ())
          }

        case Busy(workers, job) =>
          msg match {
            case Attach(worker) =>
              for {
                _ <- log.info("attached 1 worker")
                _ <- worker ! Worker.Start(job)
              } yield (Busy(worker :: workers, job), ())
            case Reset =>
              broadcastWorkers(Worker.Reset) *>
                UIO(Ready(workers), ())
            case Start(job) =>
              broadcastWorkers(Worker.Start(job)) *>
                UIO(Busy(workers, job), ())
          }
      }
    }
  }
}
