package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day19Mut extends AoC:
  def part1(lines: Vector[String]): Long =
    val (towels, patterns)              = parse(lines)
    def solve(pattern: String): Boolean =
      pattern.isEmpty || towels.flatMap(pattern.dropPrefix).exists(solve)
    patterns.count(solve)
  end part1

  override def part2(lines: Vector[String]): Long =
    val (towels, patterns)           = parse(lines)
    val cache                        = mutable.Map("" -> 1L)
    def solve(pattern: String): Long = cache.memo(pattern):
      towels.flatMap(pattern.dropPrefix).foldMap(solve)
    patterns.foldMap(solve)
  end part2

  private def parse(lines: Vector[String]): (Vector[String], Vector[String]) =
    (lines(0).split(", ").toVector, lines.drop(2))

end Day19Mut
