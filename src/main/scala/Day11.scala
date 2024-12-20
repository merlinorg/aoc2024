package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day11 extends AoC:

  def part1(lines: Vector[String]): Long = blink(lines, 25)

  def part2(lines: Vector[String]): Long = blink(lines, 75)

  private def blink(lines: Vector[String], iterations: Int): Long =
    Iterator
      .iterate(lines.head.numbers.foldMap(n => Map(n -> 1L))): kenneth =>
        kenneth.toVector.foldMap:
          case (0, count)          => Map(1L -> count)
          case (Even(a, b), count) => if (a == b) Map(a -> 2 * count) else Map(a -> count, b -> count)
          case (n, count)          => Map(n * 2024 -> count)
      .nth(iterations)
      .suml

end Day11

object Even:
  def unapply(value: Long): Option[(Long, Long)] =
    Option(value.digits).filter(_.even).map(digits => value /% (10L ** (digits / 2)))
