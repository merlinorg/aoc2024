package org.merlin.aoc2024

import scala.collection.mutable

object Day16Mut extends AoC:
  import Day16Alt.{Reindeer, findPosition, East, Position, Direction}

  def part1(lines: Vector[String]): Long = part1(lines.mkString("\n"))

  def part2(lines: Vector[String]): Long = part2(lines.mkString("\n"))

  def part1(input: String): Int =
    val (reindeer, _) = solve(input)
    reindeer.score

  def part2(input: String): Int =
    val (reindeer, queue) = solve(input)
    val paths             = mutable.Set.from(reindeer.path)
    while queue.head.score == reindeer.score do
      val next = queue.dequeue()
      if next.pos == reindeer.pos then paths.addAll(next.path)
    paths.size

  def solve(input: String): (Reindeer, mutable.PriorityQueue[Reindeer]) =
    val maze     = input.split("\n")
    val start    = maze.findPosition('S').get
    val end      = maze.findPosition('E').get
    val reindeer = Reindeer(0, start, East, Vector(start))
    val visited  = mutable.Set.empty[(Position, Direction)]
    val queue    = mutable.PriorityQueue.from(Seq(reindeer))

    while queue.head.pos != end do
      val reindeer = queue.dequeue()

      val neighbours = reindeer.neighbours.filter: next =>
        maze(next.pos) != '#' && !visited(next.pos -> next.dir)

      visited.addOne(reindeer.pos -> reindeer.dir)
      queue.addAll(neighbours)

    (queue.dequeue(), queue)

  given Ordering[Reindeer] = Ordering.by(-_.score)

end Day16Mut
