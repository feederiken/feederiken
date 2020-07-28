package feederiken.actors

import zio._, zio.actors._

import feederiken.Env

object Worker {
  sealed trait State extends Product with Serializable
  private case object Ready extends State
  private case class Busy(fiber: Fiber[Nothing, Unit]) extends State
  def initial: State = Ready

  sealed trait Commands[+_] extends Product with Serializable
  case class Start(job: Job) extends Commands[Unit]
  case object Reset extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      state match {
        case Ready =>
          msg match {
            case Reset => IO.succeed(Ready, ())
            case Start(job) =>
              for {
                fiber <- job.perform.orDie.fork
              } yield (Busy(fiber), ())
          }
        case Busy(fiber) =>
          msg match {
            case Reset =>
              for {
                _ <- fiber.interrupt
              } yield (Ready, ())
            case Start(job) =>
              for {
                _ <- fiber.interrupt
                fiber <- job.perform.orDie.fork
              } yield (Busy(fiber), ())
          }
      }
  }
}
