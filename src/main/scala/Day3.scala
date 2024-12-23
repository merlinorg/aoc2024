package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day3 extends AoC:

  val Mul1Re = """mul\((\d+),(\d+)\)""".r

  def part1(lines: Vector[String]): Long =
    lines.foldMap: line =>
      Mul1Re.findAllIn(line).toVector.foldMap(_.numbers.product)
  end part1

  val Mul2Re = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r

  def part2(lines: Vector[String]): Long =
    Mul2Re
      .findAllIn(lines.mkString)
      .toVector
      .foldLeftMap((0L, true))(_._1):
        case ((total, _), "do()")    => (total, true)
        case ((total, _), "don't()") => (total, false)
        case ((total, false), _)     => (total, false)
        case ((total, true), mul)    => (total + mul.numbers.product, true)
  end part2

end Day3
