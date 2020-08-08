package feederiken.actors

import zio._, zio.actors._

import feederiken.Env

object Saver {
  sealed trait State extends Product with Serializable
  private case object Ready extends State
  def initial: State = Ready

  sealed trait Commands[+_] extends Product with Serializable
  case class Save(result: Job.Result) extends Commands[Unit]

  object Interpreter extends Actor.Stateful[Env, State, Commands] {
    def receive[A](
        state: State,
        msg: Commands[A],
        context: Context,
    ): RIO[Env, (State, A)] =
      state match {
        case Ready =>
          msg match {
            case Save(result) =>
              for {
                _ <- feederiken.saveResult(result)
              } yield (state, ())
          }
      }
  }
}
