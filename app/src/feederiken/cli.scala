package feederiken

import com.monovore.decline._
import cats.syntax.all._
import cats.instances.list._
import cats.data.Chain

sealed abstract class Command
case class Search(j: Option[Int], prefix: Chain[Byte]) extends Command
case class Bench(j: Option[Int], n: Int) extends Command

object CLI {
  import Opts._

  private val j = option[Int]("parallelism-level", "# of concurrent threads to use", short="j")
    .validate("j must be positive")(_ > 0)
    .orNone
  private val n = option[Int]("iterations", "how many keys to generate for benchmarking", short="n")
    .withDefault(10000)
    .validate("n must be positive")(_ > 0)
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

  private def bench =
    Command[Bench]("bench", "benchmark CPU hashrate") {
      (j, n).mapN(Bench)
    }

  private def search =
    Command[Search]("search", "search for a vanity key") {
      (j, prefix).mapN(Search)
    }

  def top =
    Command[Command]("feederiken", "vanity PGP key generator") {
      subcommands(search, bench)
    }
}
