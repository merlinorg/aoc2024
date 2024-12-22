package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day22 extends AoC:
  def part1(lines: Vector[String]): Long =
    lines.foldMap: line =>
      Iterator.iterate(line.toLong)(next).nth(2000)
  end part1

  def part2(lines: Vector[String]): Long =
    val sequenceTotals = lines.foldMap: line =>
      val secrets  = Iterator.iterate(line.toLong)(next).take(2000).map(_ % 10)
      secrets.sliding(5).foldRight(Map.empty[(Long, Long, Long, Long), Long]):
        case (Seq(a, b, c, d, e), map) => map + ((b - a, c - b, d - c, e - c) -> e)
        case (_, map) => map
    sequenceTotals.values.max
  end part2

  def next(secret0: Long): Long =
    val secret1 = ((secret0 * 64) ^ secret0) % 16777216
    val secret2 = ((secret1 / 32) ^ secret1) % 16777216
    ((secret2 * 2048) ^ secret2) % 16777216

end Day22
