package feederiken.actors

import zio._, zio.actors._

import feederiken.core._, feederiken.messages._

object FeederikenSaver {
  import Saver._

  sealed trait State extends Product with Serializable
  private case object Ready extends State
  val initial: State = Ready

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
                _ <- saveResult(result)
              } yield (state, ())
          }
      }
  }
}
