package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day20 extends AoC:
  def part1(maze: Vector[String]): Long =
    solve(maze, 2)
  end part1

  override def part2(maze: Vector[String]): Long =
    solve(maze, 20)
  end part2

  def solve(maze: Vector[String], cheatable: Long): Long =
    val start     = maze.find('S')
    val picos     = if maze.length == 15 then 50 else 100
    val distances = walk(maze, start, Map(start -> 0L))

    distances.toVector.allPairs.count:
      case ((loc0, dst0), (loc1, dst1)) =>
        val dist = loc0.manhattan(loc1)
        (dist <= cheatable) && ((dst1 - dst0).abs - dist >= picos)

  @tailrec def walk(maze: Vector[String], loc: Loc, distances: Map[Loc, Long]): Map[Loc, Long] =
    loc.adjacents.find(loc => !maze.is(loc, '#') && !distances.contains(loc)) match
      case Some(nxt) => walk(maze, nxt, distances + (nxt -> distances.size))
      case None      => distances

end Day20
