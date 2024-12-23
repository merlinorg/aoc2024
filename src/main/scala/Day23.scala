package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day23 extends AoC:
  def part1(lines: Vector[String]): Long =
    parse(lines).cliques(3).toSet.count(_.exists(_.startsWith("t")))
  end part1

  def part2(lines: Vector[String]): String =
    val network = parse(lines)
    (network.values.map(_.size).max to 3 by -1).findMap: size =>
      network.cliques(size).map(_.toVector.sorted.mkString(",")).nextOption()
  end part2

  type Network = Map[String, Set[String]]

  extension (network: Network)
    def connected(subnet: Vector[String]): Boolean = subnet.allPairs.forall(connected.tupled)
    def connected(a: String, b: String): Boolean   = network(a)(b)

    def cliques(size: Int): Iterator[Set[String]] =
      network.valuesIterator.flatMap: subnet =>
        subnet.toVector
          .combinations(size)
          .filter(connected)
          .map(_.toSet)

  def parse(lines: Vector[String]): Network =
    lines
      .collect:
        case s"$a-$b" => (a, b)
      .foldMap: (a, b) =>
        Map(a -> Set(a, b), b -> Set(a, b))

end Day23
