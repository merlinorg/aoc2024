package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day8 extends AoC:

  override def part1(lines: Vector[String]): Long =
    parse(lines).foldMap:
      case (total, numbers) =>
        def solvable(numbers: Vector[Long], sum: Long = 0): Boolean = numbers match
          case num +: rest => solvable(rest, sum + num) || solvable(rest, sum * num)
          case _           => sum == total
        solvable(numbers) ?? total
  end part1

  override def part2(lines: Vector[String]): Long =
    parse(lines).foldMap:
      case (total, numbers) =>
        def solvable(numbers: Vector[Long], sum: Long = 0): Boolean = numbers match
          case num +: rest => solvable(rest, sum + num) || solvable(rest, sum * num) || solvable(rest, sum || num)
          case _           => sum == total
        solvable(numbers) ?? total
  end part2

  private def parse(lines: Vector[String]): Vector[(Long, Vector[Long])] =
    lines.map:
      case s"$a: $b" => a.toLong -> b.numbers

  extension (self: Long) def ||(n: Long): Long = s"$self$n".toLong

end Day8
