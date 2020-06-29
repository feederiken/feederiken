package feederiken

import com.monovore.decline._
import cats.syntax.all._
import cats.instances.list._
import cats.data.Chain
import cats.data.NonEmptyList
import java.nio.file.Path

sealed abstract class Command
case class Search(j: Option[Int], prefix: Chain[Byte]) extends Command
case class Bench(j: Option[Int], n: Int) extends Command
case class Node(configFile: Path, j: Option[Int]) extends Command
case class Coordinator(
    prefix: Chain[Byte],
    nodes: NonEmptyList[String],
    localNode: Boolean,
) extends Command

object CLI {
  import Opts._

  private val j = option[Int]("parallelism-level", "# of concurrent threads to use", short="j")
    .validate("j must be positive")(_ > 0)
    .orNone
  private val n = option[Int]("iterations", "how many keys to generate for benchmarking", short="n")
    .withDefault(10000)
    .validate("n must be positive")(_ > 0)
  private val nodes = arguments[String]("nodes")
  private val localNode =
    flag("no-local-node", "don't start a search node on this machine").orTrue
  private val prefix = option[String]("prefix", "key prefix to look for (hex)")
    .withDefault("feed")
    .mapValidated {
      _.filterNot(_.isWhitespace).grouped(2).toList.foldMap { s =>
        try Chain(Integer.parseUnsignedInt(s, 16).toByte).valid
        catch {
          case _: NumberFormatException => s"Invalid byte: $s".invalidNel
        }
      }
    }
  private val configFile = argument[Path]("config_file")

  private def bench =
    Command[Bench]("bench", "benchmark CPU hashrate") {
      (j, n).mapN(Bench)
    }

  private def search =
    Command[Search]("search", "search for a vanity key") {
      (j, prefix).mapN(Search)
    }

  private def node =
    Command[Node]("node", "serve as a node in a distributed search") {
      (configFile, j).mapN(Node)
    }

  private def coordinator =
    Command[Coordinator]("coordinator", "coordinate a distributed search") {
      (prefix, nodes, localNode).mapN(Coordinator)
    }

  def top =
    Command("feederiken", "vanity PGP key generator") {
      Opts.subcommands(search, bench, node, coordinator)
    }
}
