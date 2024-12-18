package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.immutable.Queue
import scala.collection.parallel.CollectionConverters.*

object Day18 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (size, take, walls) = parse(lines)
    Iterator.iterate(Search(walls.take(take), size))(_.nextState).findMap(_.solution1)
  end part1

  override def part2(lines: Vector[String]): Loc =
    val (size, take, walls) = parse(lines)
    walls.apply:
      (take to walls.size).toVector.par
        .filter: n =>
          Iterator.iterate(Search(walls.take(n), size))(_.nextState).findMap(_.solution2)
        .min - 1
  end part2

  case class Search(walls: Set[Loc], end: Loc, size: Int, queue: Queue[State], visited: Set[Loc]):
    def nextState: Search =
      val (head, tail) = queue.dequeue
      val adjacent     = head.neighbours.filter(next => next.loc >=< size && !walls(next.loc) && !visited(next.loc))
      copy(queue = tail.enqueueAll(adjacent), visited = visited ++ adjacent.map(_.loc))

    def solution1: Option[Long] =
      Option.when(queue.head.loc == end)(queue.head.steps)

    def solution2: Option[Boolean] =
      Option.when(queue.isEmpty || queue.head.loc == end)(queue.isEmpty)

  object Search:
    def apply(walls: Vector[Loc], size: Int): Search =
      Search(walls.toSet, Loc(size - 1, size - 1), size, Queue(State(Origin, 0)), Set(Origin))

  case class State(loc: Loc, steps: Long):
    def neighbours: Vector[State] =
      CardinalDirections.map(dir => State(loc + dir, steps + 1))

  private def parse(lines: Vector[String]): (Int, Int, Vector[Loc]) = (
    if lines.length == 25 then 7 else 71,
    if lines.length == 25 then 12 else 1024,
    lines.collect:
      case s"${L(x)},${L(y)}" => Loc(x, y)
  )

end Day18
