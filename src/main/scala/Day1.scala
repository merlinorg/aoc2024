package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day1 extends AoC:

  override def part1(lines: Vector[String]): Long =
    val (left, right) = lines.pairs.unzip
    left.sorted.zip(right.sorted).foldMap: (l, r) =>
      (l - r).abs
  end part1

  override def part2(lines: Vector[String]): Long =
    val (left, right) = lines.pairs.unzip
    left.foldMap: digit =>
      digit * right.count(_ == digit)
  end part2

  extension (self: Vector[String])
    def pairs: Vector[(Long, Long)] =
      for
        line <- self
        numbers = line.numbers
      yield (numbers(0), numbers(1))

end Day1