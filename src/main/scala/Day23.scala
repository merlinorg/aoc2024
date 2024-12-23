package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day23 extends AoC:
  def part1(lines: Vector[String]): Long =
    cliques(parse(lines), 3).toSet.count(_.exists(_.startsWith("t")))
  end part1

  def part2(lines: Vector[String]): String =
    val network = parse(lines)
    (network.values.map(_.size).max to 3 by -1).toVector.findMap: max =>
      cliques(network, max).headOption.map(_.toVector.sorted.mkString(","))
  end part2

  type Network = Map[String, Set[String]]

  def cliques(network: Network, size: Int): Iterable[Set[String]] =
    network
      .flatMap: (origin, connected) =>
        connected.toVector
          .combinations(size - 1)
          .filter(network.connected)
          .map: subnet =>
            Set(origin) ++ subnet

  def parse(lines: Vector[String]): Network =
    lines
      .collect:
        case s"$a-$b" => (a, b)
      .foldMap: (a, b) =>
        Map(a -> Set(b), b -> Set(a))

  extension (network: Network)
    def connected(subnet: Vector[String]): Boolean = subnet.allPairs.forall(connected)
    def connected(pair: (String, String)): Boolean = network(pair(0))(pair(1))

end Day23
