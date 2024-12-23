package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day23 extends AoC:
  def part1(lines: Vector[String]): Long =
    cliques(parse(lines), 3).count(_.exists(_.startsWith("t")))
  end part1

  def part2(lines: Vector[String]): String =
    val network = parse(lines)
    (network.values.map(_.size).max to 3 by -1).toVector.findMap: max =>
      cliques(network, max).headOption.map(_.toVector.sorted.mkString(","))
  end part2

  type Network = Map[String, Set[String]]

  def cliques(network: Network, size: Int): Set[Set[String]] =
    network
      .flatMap: (computer, connected) =>
        connected.toVector
          .combinations(size - 1)
          .filter: combo =>
            combo.combinations(2).forall(pair => network(pair(0))(pair(1)))
          .map: combo =>
            Set(computer) ++ combo
      .toSet

  def parse(lines: Vector[String]): Network =
    lines
      .collect:
        case s"$a-$b" => (a, b)
      .foldMap: (a, b) =>
        Map(a -> Set(b), b -> Set(a))

end Day23
