package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day13 extends AoC:

  def part1(lines: Vector[String]): Long = parse(lines).foldMap(_.cost)

  def part2(lines: Vector[String]): Long =
    parse(lines).map(g => g.copy(x = g.x + 10000000000000L, y = g.y + 10000000000000L)).foldMap(_.cost)

  def parse(lines: Vector[String]): Vector[Game] =
    lines
      .grouped(4)
      .toVector
      .collect:
        case s"Button A: X+$ax, Y+$ay" +: s"Button B: X+$bx, Y+$by" +: s"Prize: X=$x, Y=$y" +: _ =>
          Game(ax.toLong, ay.toLong, bx.toLong, by.toLong, x.toLong, y.toLong)

  case class Game(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long):
    // This seems naïve as it does not search for the fewest where there are multiple solutions,
    // but it works for the given puzzles.
    // n.ax + m.bx = x, n.ay + m.by = y
    // => n = (x - m.bx) / ax, n = (y - m.by) / ay
    // => ay * (x - mbx) = ax * (y - m.by)
    // => ay.x - ay.m.bx = ax.y - ax.m.by
    // => ax.m.by - ay.m.bx = ax.y - ay.x
    // => m = (ax.y - ay.x) / (ax.by - ay.bx), n = (bx.y - by.x) / (bx.ay - by.ax)
    def solution: Option[(Long, Long)] =
      val (anum, aden) = (bx * y - by * x, bx * ay - by * ax)
      val (bnum, bden) = (ax * y - ay * x, ax * by - ay * bx)
      Option.when(bden != 0 && bnum % bden == 0 && aden != 0 && anum % aden == 0):
        anum / aden -> bnum / bden

    def cost: Long = solution.cata(_ * 3 + _, 0)

end Day13
