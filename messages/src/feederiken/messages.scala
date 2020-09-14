package feederiken.messages

import zio._

import feederiken.core._, feederiken.pgp._

/**
  * Serializable representation of a key search.
  */
case class Job(
    goal: Chunk[Byte],
    mode: Mode,
    minScore: Int,
    maxScore: Int,
    collector: Collector.Ref,
) {
  def score(kp: Job.Result): Int = mode.score(goal, kp.fingerprint)
}

object Job {
  type Result = DatedKeyPair
}

trait AutoRef {
  type Commands[+_]
  type Ref = zio.actors.ActorRef[Commands]
}

object Collector extends AutoRef {
  sealed trait Commands[+_] extends Product with Serializable
  case class Start(job: Job) extends Commands[Unit]
  case class Process(result: Job.Result) extends Commands[Unit]
}

object Dispatcher extends AutoRef {
  sealed trait Commands[+_] extends Product with Serializable
  case class Attach(worker: Worker.Ref) extends Commands[Unit]
  case object Reset extends Commands[Unit]
  case class Start(job: Job) extends Commands[Unit]
}

object Worker extends AutoRef {
  sealed trait Commands[+_] extends Product with Serializable
  case class Start(job: Job) extends Commands[Unit]
  case object Reset extends Commands[Unit]
}

object Saver extends AutoRef {
  sealed trait Commands[+_] extends Product with Serializable
  case class Save(result: Job.Result) extends Commands[Unit]
}
