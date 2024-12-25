package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day25 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (locks, keys) = parse(lines)
    locks.foldMap: lock =>
      keys.count: key =>
        key.locations.forall: loc =>
          key.is(loc, '.') || lock.is(loc, '.')
  end part1

  def part2(lines: Vector[String]): Long = 0

  def parse(lines: Vector[String]): (Vector[Vector[String]], Vector[Vector[String]]) =
    val blocks = lines.selectSplit(_.nonEmpty).map(_.toVector).toVector
    blocks.partition(block => block.head.startsWith("#"))
end Day25
