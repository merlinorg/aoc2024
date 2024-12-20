package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day20 extends AoC:
  def part1(maze: Vector[String]): Long = solve(maze, 2)

  def part2(maze: Vector[String]): Long = solve(maze, 20)

  def solve(maze: Vector[String], cheat: Long): Long =
    val start = maze.find('S')
    val picos = if maze.length == 15 then 50 else 100

    val path = Vector((start, 0)) ++ Iterator.unfold((start, start, 1)): (prev, cur, steps) =>
      cur.adjacents
        .find(loc => !maze.is(loc, '#') && loc != prev)
        .map(loc => ((loc, steps), (cur, loc, steps + 1)))

    path.tails.foldCollect:
      case (loc0, dst0) +: tail =>
        tail.drop(picos).count: (loc1, dst1) =>
          val dist = loc0.manhattan(loc1)
          (dist <= cheat) && (dst1 - dst0 - dist >= picos)

end Day20
