package feederiken

import com.monovore.decline._
import cats.syntax.all._
import cats.instances.list._
import cats.data._
import cats.instances.tuple


/**
 * A mode of rating key fingerprints. An instance allows the brute-forcing process to compare its finding to the goal of the search.
 *
 * Laws
 * - `score` is commutative.
 * - `score` returns an integer less than or equal to `maxScore` with the appropriate arguments.
 * - There must exist a sequence that scores `maxScore`.
 */
sealed abstract class Mode extends Product with Serializable {
  /**
   * Rate the similarity of two byte sequences according to the criteria. Both sequences may be of any length. This operation must be commutative.
   */
  def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int
  /**
   * Compute the highest possible similarity score according to the criteria.
   */
  def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int
}

object Mode {
  /**
    * A mode to search for a key with a given prefix.
    */
  case object Prefix extends Mode {
    override def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int =
      (fpr1.iterator zip fpr2).takeWhile(x => x._1 == x._2).length
    override def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int =
      fpr1.length min fpr2len
  }

  /**
    * A mode to search for a key with a given suffix.
    */
  case object Suffix extends Mode {
    override def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int =
      (fpr1.reverseIterator zip fpr2.reverseIterator).takeWhile(x => x._1 == x._2).length
    override def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int =
      fpr1.length min fpr2len
  }

  implicit object ModeArgument extends Argument[Mode] {
    override def read(string: String): ValidatedNel[String, Mode] =
      string.toLowerCase match {
        case "prefix" => Prefix.valid
        case "suffix" => Suffix.valid
        case unknown => s"No such mode: $unknown".invalidNel
      }

    override def defaultMetavar: String = "mode"
  }
}

sealed abstract class Command extends Product with Serializable
case class Search(j: Option[Int], goal: Chain[Byte], mode: Mode) extends Command
case class Bench(j: Option[Int], n: Int) extends Command

object CLI {
  import Opts._

  private val j = option[Int]("parallelism-level", "# of concurrent threads to use", short="j")
    .validate("j must be positive")(_ > 0)
    .orNone
  private val n = option[Int]("iterations", "how many keys to generate for benchmarking", short="n")
    .withDefault(10000)
    .validate("n must be positive")(_ > 0)
  private val goal = argument[String]("goal")
    .withDefault("feed")
    .mapValidated {
      _.filterNot(_.isWhitespace).grouped(2).toList.foldMap { s =>
        try Chain(Integer.parseUnsignedInt(s, 16).toByte).valid
        catch {
          case _: NumberFormatException => s"Invalid byte: $s".invalidNel
        }
      }
    }
  private val mode = argument[Mode]("mode").withDefault(Mode.Prefix)

  private def bench =
    Command[Bench]("bench", "benchmark CPU hashrate") {
      (j, n).mapN(Bench)
    }

  private def search =
    Command[Search]("search", "search for a valid PGP key with a fingerprint matching the goal according to the mode") {
      (j, goal, mode).mapN(Search)
    }

  def top =
    Command[Command]("feederiken", "Vanity PGP key generator") {
      subcommands(search, bench)
    }
}
