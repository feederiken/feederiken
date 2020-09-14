package feederiken

import feederiken.core._

import com.monovore.decline._
import cats.syntax.all._
import cats.instances.list._
import cats.data._
import java.nio.file.Path

sealed abstract class Command extends Product with Serializable {
  def j: Option[Int]
}
case class Search(
    j: Option[Int],
    configFile: Option[Path],
    localSearch: Boolean,
    goal: Chain[Byte],
    mode: Mode,
    minScore: Option[Int],
    maxScore: Option[Int],
) extends Command
case class Bench(j: Option[Int], n: Int) extends Command
case class Node(
    j: Option[Int],
    configFile: Path,
    nodeName: String,
    dispatcherPath: String,
) extends Command

object CLI {
  import Opts._

  implicit object ModeArgument extends Argument[Mode] {
    override def read(string: String): ValidatedNel[String, Mode] =
      string.toLowerCase match {
        case "prefix" => Mode.Prefix.valid
        case "suffix" => Mode.Suffix.valid
        case unknown  => s"No such mode: $unknown".invalidNel
      }

    override def defaultMetavar: String = "mode"
  }

  private val j = option[Int](
    "parallelism-level",
    "# of concurrent threads to use",
    short = "j",
  ).validate("j must be positive")(_ > 0).orNone
  private val n = option[Int](
    "iterations",
    "how many keys to generate for benchmarking",
    short = "n",
  ).withDefault(1000000)
    .validate("n must be positive")(_ > 0)
  private val dispatcherPath = argument[String]("dispatcher-path")
  private val localSearch =
    flag(
      "no-local-search",
      "don't search on this machine, rely on remote nodes",
    ).orTrue
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
  private val configFileO =
    option[Path]("config-file", "actor system config file").orNone
  private val configFileA = argument[Path]("config_file")
  private val nodeName = argument[String]("node_name")
  private val mode = argument[Mode]("mode").withDefault(Mode.Prefix)
  private val minScore = option[Int]("min-score", "lower bound score to search")
    .validate("min-score must be positive")(_ > 0)
    .orNone
  private val maxScore = option[Int]("max-score", "upper bound score to search")
    .validate("max-score must be positive")(_ > 0)
    .orNone

  private def bench =
    Command[Bench]("bench", "benchmark CPU hashrate") {
      (j, n).mapN(Bench)
    }

  private def search =
    Command[Search](
      "search",
      "search for a valid PGP key with a fingerprint matching the goal according to the mode",
    ) {
      (j, configFileO, localSearch, goal, mode, minScore, maxScore).mapN(Search)
    }

  private def node =
    Command[Node]("node", "serve as a node in a distributed search") {
      (j, configFileA, nodeName, dispatcherPath).mapN(Node)
    }

  def top =
    Command[Command]("feederiken", "Vanity PGP key generator") {
      subcommands(search, bench, node)
    }
}
