package feederiken.actors

import zio._

import feederiken._

object Job {
  type Result = pgp.DatedKeyPair
}

/**
  * Serializable representation of a key search.
  */
case class Job(
    goal: Chunk[Byte],
    mode: Mode,
    minScore: Int,
    maxScore: Int,
    collector: Collector,
) {
  def score(kp: Job.Result): Int = mode.score(goal, kp.fingerprint)
  def perform: RIO[Env, Unit] =
    parallelize {
      genCandidates.filter { kp =>
        score(kp) >= minScore
      }
    } foreach { result =>
      collector ! Collector.Process(result)
    }
}
