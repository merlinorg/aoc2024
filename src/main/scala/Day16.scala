package org.merlin.aoc2024

import scala.collection.immutable.TreeMap

object Day16 extends AoC:
  override def part1(lines: Vector[String]): Long =
    Iterator.iterate(ReindeerMaze(lines))(_.nextState).findMap(_.solution1)

  override def part2(lines: Vector[String]): Long =
    Iterator.iterate(ReindeerMaze(lines))(_.nextState).findMap(_.solution2)

  case class Reindeer(cost: Long, pos: Loc, dir: Dir, path: Vector[Loc]):
    def nextReindeers: Vector[Reindeer] = Vector(
      Reindeer(cost + 1, pos + dir, dir, path :+ (pos + dir)),
      Reindeer(cost + 1000, pos, dir.cw, path),
      Reindeer(cost + 1000, pos, dir.ccw, path)
    )

  case class ReindeerMaze(
    maze: Vector[String],
    end: Loc,
    visited: Set[(Loc, Dir)],
    queue: TreeMap[Long, Vector[Reindeer]]
  ):
    private val head = queue.valuesIterator.next

    def nextState: ReindeerMaze =
      val (nextVisited, nextQueue) = head.foldLeft((visited, queue - head.head.cost)):
        case ((visited, queue), reindeer) =>
          val nextQueue = reindeer.nextReindeers
            .filter: reindeer =>
              !maze.is(reindeer.pos, '#') && !visited(reindeer.pos -> reindeer.dir)
            .foldLeft(queue): (queue, reindeer) =>
              queue.updatedWith(reindeer.cost):
                case Some(reindeers) => Some(reindeers :+ reindeer)
                case None            => Some(Vector(reindeer))
          (visited + (reindeer.pos -> reindeer.dir), nextQueue)
      ReindeerMaze(maze, end, nextVisited, nextQueue)

    def solution1: Option[Long] =
      head.find(_.pos == end).map(_.cost)

    def solution2: Option[Long] =
      Option.when(head.exists(_.pos == end)):
        head.filter(_.pos == end).flatMap(_.path).distinct.size

  object ReindeerMaze:
    def apply(lines: Vector[String]): ReindeerMaze =
      val (start, end) = (lines.find('S'), lines.find('E'))
      new ReindeerMaze(lines, end, Set.empty, TreeMap(0L -> Vector(Reindeer(0, start, Dir.E, Vector(start)))))

end Day16
