package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day1 extends AoC:

  def part1(lines: Vector[String]): Long =
    val (left, right) = lines.pairs.unzip
    left.sorted
      .zip(right.sorted)
      .foldMap(_ |-| _)
  end part1

  def part2(lines: Vector[String]): Long =
    val (left, right) = lines.pairs.unzip
    left.foldMap: digit =>
      digit * right.count(_ == digit) // quadratic shame
  end part2

  extension (self: Vector[String])
    def pairs: Vector[(Long, Long)] =
      self.map:
        case s"$a   $b" => (a.toLong, b.toLong)

end Day1
