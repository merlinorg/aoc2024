package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day3 extends AoC:

  val Mul1Re = """mul\((\d+),(\d+)\)""".r

  override def part1(lines: Vector[String]): Long =
    lines.foldMap: line =>
      Mul1Re.findAllIn(line).toVector.foldMap(_.numbers.product)
  end part1

  val Mul2Re = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r

  override def part2(lines: Vector[String]): Long =
    Mul2Re
      .findAllIn(lines.mkString)
      .toVector
      .foldLeft((0L, true)):
        case ((total, _), "do()")    => (total, true)
        case ((total, _), "don't()") => (total, false)
        case ((total, false), _)     => (total, false)
        case ((total, true), mul)    => (total + mul.numbers.product, true)
      ._1
  end part2

end Day3
