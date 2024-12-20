package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day5 extends AoC:

  def part1(lines: Vector[String]): Long =
    val (rules, updates) = parse(lines)
    updates.filter(valid(_, rules)).foldMap(_.middle)
  end part1

  def part2(lines: Vector[String]): Long =
    val (rules, updates) = parse(lines)
    updates
      .filterNot(valid(_, rules))
      .foldMap(_.sortWith((a, b) => rules.contains(a -> b)).middle)
  end part2

  private type Rules  = Vector[(Long, Long)]
  private type Update = Vector[Long]

  private def valid(update: Update, rules: Rules): Boolean =
    rules.forall: (a, b) =>
      !update.contains(a) || !update.contains(b) || update.indexOf(a) < update.indexOf(b)

  private def parse(lines: Vector[String]): (Rules, Vector[Update]) =
    lines
      .span(_.nonEmpty)
      .bimap(
        _.map({ case s"$a|$b" => (a.toLong, b.toLong) }),
        _.tail.map(_.numbers)
      )

end Day5
