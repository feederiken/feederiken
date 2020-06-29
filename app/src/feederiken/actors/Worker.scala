package feederiken.actors

import zio._
import zio.actors._

import feederiken.Env

object Worker {
  sealed trait State
  case object Ready extends State
  case class Busy(fiber: Fiber[Any, Any]) extends State

  sealed trait Commands[+_]
  case class Start(job: ZIO[Env, Any, Any]) extends Commands[Unit]
  case object Poll extends Commands[Option[Exit[Any, Any]]]
  case object Reset extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      (state, msg) match {
        case (Ready, Start(job)) =>
          for {
            fiber <- job.fork
          } yield (Busy(fiber), ())
        case (Busy(fiber), Poll) =>
          for {
            exit <- fiber.poll
          } yield (state, exit)
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
