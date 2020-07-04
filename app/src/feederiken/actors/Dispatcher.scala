package feederiken.actors

import zio._
import zio.actors._

import feederiken.Env
import zio.logging.log

object Dispatcher {
  type E = Any
  type A = Any

  sealed trait State {
    val workers: List[Worker]
  }
  private case class Ready(workers: List[Worker]) extends State
  private case class Busy(
      workers: List[Worker],
      job: ZIO[Env, E, A],
      p: Promise[E, A],
  ) extends State

  def initial: State = Ready(Nil)

  sealed trait Commands[+_]
  case class Attach(worker: Worker) extends Commands[Unit]
  case object Reset extends Commands[Unit]
  case class Start(job: ZIO[Env, E, A], p: Promise[E, A]) extends Commands[Unit]
  case class Done(exit: Exit[E, A]) extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] = {
      def broadcastWorkers(msg: Worker.Commands[Any]) =
        IO.foreachPar_(state.workers) { _ ? msg }
      context.self >>= { (self: Dispatcher) =>
        (state, msg) match {
          case (Ready(workers), Attach(worker)) =>
            UIO(Ready(worker :: workers), ())
          case (Ready(_), Reset) =>
            UIO(initial, ())
          case (Ready(workers), Start(job, p)) =>
            broadcastWorkers(Worker.Start(job, self)) *>
              UIO(Busy(workers, job, p), ())
          case (Ready(_), Done(_)) =>
            UIO(state, ())

          case (busy @ Busy(workers, job, _), Attach(worker)) =>
            { worker ? Worker.Start(job, self) } *>
              UIO(busy.copy(workers = worker :: workers), ())
          case (Busy(workers, _, _), Reset) =>
            broadcastWorkers(Worker.Reset) *>
              UIO(initial, ())
          case (Busy(workers, _, p), Done(exit)) =>
            broadcastWorkers(Worker.Reset) *>
              p.done(exit) *>
              UIO(Ready(workers), ())

          case _ =>
            IO.fail(new IllegalStateException(s"$state cannot perform $msg"))
        }
      }
    }
  }
}
