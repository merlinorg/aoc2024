package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day19 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (towels, patterns) = parse(lines)

    patterns.count(Y[String, Boolean]((self, pattern) => pattern.isEmpty || towels.exists: towel =>
      pattern.startsWith(towel) && self(pattern.substring(towel.length)), _))
  end part1

  override def part2(lines: Vector[String]): Long =
    val (towels, patterns) = parse(lines)
    patterns.foldMap: pattern =>
      def loop(pattern: String, total: Long, cache: Map[String, Long]): (Long, Map[String, Long]) =
        cache.get(pattern) match
          case Some(count) => (total + count, cache)
          case _           =>
            val (count, cache2) = towels.foldLeft(0L -> cache):
              case ((count, cache), towel) =>
                if !pattern.startsWith(towel) then (count, cache)
                else loop(pattern.substring(towel.length), count, cache)
            (total + count, cache2 + (pattern -> count))
      loop(pattern, 0L, Map("" -> 1L))._1
  end part2

  private def parse(lines: Vector[String]): (Vector[String], Vector[String]) = (
    lines(0).split(", ").toVector,
    lines.drop(2)
  )

end Day19
