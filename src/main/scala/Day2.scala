package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day2 extends AoC:

  def part1(lines: Vector[String]): Long =
    lines.numbers.count(safe)
  end part1

  def part2(lines: Vector[String]): Long =
    lines.numbers.count: numbers =>
      safe(numbers) || numbers.indices.exists: index =>
        safe(numbers.splice(index, 1))
  end part2

  private def safe(numbers: Vector[Long]): Boolean =
    (numbers == numbers.sorted || numbers == numbers.sorted.reverse) &&
      numbers.adjacentPairs.forall: (a, b) =>
        (a |-| b) >=< (1, 4)

end Day2
