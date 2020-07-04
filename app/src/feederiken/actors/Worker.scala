package feederiken.actors

import zio._
import zio.actors._

import feederiken.Env

object Worker {
  type E = Any
  type A = Any
  sealed trait State
  private case object Ready extends State
  private case class Busy(fiber: Fiber[Nothing, Unit]) extends State
  def initial: State = Ready

  sealed trait Commands[+_]
  case class Start(job: ZIO[Env, E, A], dispatcher: Dispatcher)
      extends Commands[Unit]
  case object Reset extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      (state, msg) match {
        case (Ready, Start(job, dispatcher)) =>
          for {
            fiber <- job.run.>>= { dispatcher ? Dispatcher.Done(_) }.orDie.fork
          } yield (Busy(fiber), ())
        case (Busy(fiber), Reset) =>
          for {
            _ <- fiber.interrupt
          } yield (Ready, ())
        case (_, Reset) => UIO(Ready, ())
        case _ =>
          IO.fail(new IllegalStateException(s"$state cannot perform $msg"))
      }
  }
}
