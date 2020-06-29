package feederiken.actors

import zio._
import zio.actors._
import zio.duration._
import java.io.File
import zio.logging.log

trait FeederikenSystem {
  protected val as: ActorSystem
  val worker: Worker
  def select[F[+_]](path: String) = as.select(path)

  def executeOn[E, A](
      workers: Seq[Worker]
  )(job: ZIO[feederiken.Env, E, A]): RIO[feederiken.Env, Exit[E, A]] =
    (for {
      _ <- IO.foreach_(workers)(_ ! Worker.Start(job))
      _ <- log.info("Job dispatched")
      exit <- (for {
          exits <- IO.foreach(workers)(_ ? Worker.Poll)
          exit <- ZIO.getOrFail(exits.collectFirst { case Some(e) => e })
        } yield exit).retry(Schedule.fixed(1.second))
    } yield exit.asInstanceOf[Exit[E, A]])
      .ensuring(IO.foreach_(workers)(_ ! Worker.Reset).orDie)
}
object FeederikenSystem {
  def start(name: String, configFile: Option[File] = None) =
    for {
      _as <- ActorSystem(name, configFile)
      sup = actors.Supervisor.none
      _worker <- _as.make("worker", sup, Worker.Ready, Worker.Interpreter)
    } yield new FeederikenSystem {
      override val as = _as
      override val worker = _worker
    }
}
