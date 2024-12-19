package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day19 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (towels, patterns) = parse(lines)
    patterns.count(
      Y[String, Boolean](
        (self, pattern) =>
          pattern.isEmpty || towels.exists: towel =>
            pattern.startsWith(towel) && self(pattern.substring(towel.length)),
        _
      )
    )
  end part1

  override def part2(lines: Vector[String]): Long =
    val (allTowels, patterns) = parse(lines)
    Iterator
      .iterate((Vector(patterns.head -> allTowels), Map("" -> 1L), patterns, 0L)):
        case ((pattern, _) +: tail, cache, patterns, total) if cache.contains(pattern)                 =>
          (tail, cache, patterns, total)
        case ((pattern, towel +: towels) +: tail, cache, patterns, total) if pattern.startsWith(towel) =>
          ((pattern.substring(towel.length), allTowels) +: (pattern, towels) +: tail, cache, patterns, total)
        case ((pattern, _ +: towels) +: tail, cache, patterns, total)                                  =>
          ((pattern, towels) +: tail, cache, patterns, total)
        case ((pattern, _) +: tail, cache, patterns, total)                                            =>
          val subtotal = allTowels.foldMap: towel =>
            if pattern.startsWith(towel) then cache(pattern.substring(towel.length)) else 0
          (tail, cache + (pattern -> subtotal), patterns, total)
        case (_, cache, pattern +: next +: patterns, total)                                            =>
          (Vector(next -> allTowels), cache, next +: patterns, total + cache(pattern))
        case (_, cache, pattern +: patterns, total)                                                    =>
          (Vector.empty, cache, patterns, total + cache(pattern))
        case done                                                                                      => done
      .findMap:
        case (_, _, patterns, total) => Option.when(patterns.isEmpty)(total)
  end part2

  private def parse(lines: Vector[String]): (Vector[String], Vector[String]) =
    (lines(0).split(", ").toVector, lines.drop(2))

end Day19
