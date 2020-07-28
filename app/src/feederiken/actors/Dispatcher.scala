package feederiken.actors

import zio._, zio.actors._

import feederiken.Env

object Dispatcher {
  sealed trait State extends Product with Serializable {
    val workers: List[Worker]
  }
  private case class Ready(workers: List[Worker]) extends State
  private case class Busy(
      workers: List[Worker],
      currentJob: Job,
  ) extends State

  def initial: State = Ready(Nil)

  sealed trait Commands[+_] extends Product with Serializable
  case class Attach(worker: Worker) extends Commands[Unit]
  case object Reset extends Commands[Unit]
  case class Start(job: Job) extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Any, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Any, (State, A)] = {
      def broadcastWorkers(msg: Worker.Commands[Any]) =
        IO.foreachPar_(state.workers) { _ ! msg }
      state match {
        case Ready(workers) =>
          msg match {
            case Attach(worker) =>
              UIO(Ready(worker :: workers), ())
            case Reset =>
              UIO(state, ())
            case Start(job) =>
              broadcastWorkers(Worker.Start(job)) *>
                UIO(Busy(workers, job), ())
          }

        case Busy(workers, job) =>
          msg match {
            case Attach(worker) =>
              { worker ! Worker.Start(job) } *>
                UIO(Busy(worker :: workers, job), ())
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
