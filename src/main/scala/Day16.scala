package org.merlin.aoc2024

import scala.collection.immutable.TreeMap

object Day16 extends AoC:
  def part1(lines: Vector[String]): Long =
    Iterator.iterate(ReindeerMaze(lines))(_.nextState).findMap(_.solution1)

  def part2(lines: Vector[String]): Long =
    Iterator.iterate(ReindeerMaze(lines))(_.nextState).findMap(_.solution2)

  case class Reindeer(cost: Long, pos: Loc, dir: Dir, path: Vector[Loc]):
    def neighbours: Vector[Reindeer] =
      Reindeer(cost + 1, pos + dir, dir, path :+ (pos + dir)) +: Vector(dir.cw, dir.ccw).map: rot =>
        Reindeer(cost + 1001, pos + rot, rot, path :+ (pos + rot))

  case class ReindeerMaze(
    maze: Vector[String],
    end: Loc,
    visited: Set[(Loc, Dir)],
    queue: TreeMap[Long, Vector[Reindeer]]
  ):
    private def headValues = queue.head(1)

    def nextState: ReindeerMaze =
      headValues.foldLeft(copy(queue = queue.tail)): (acc, reindeer) =>
        acc.copy(
          visited = acc.visited + (reindeer.pos -> reindeer.dir),
          queue = reindeer.neighbours
            .filter: neighbour =>
              !maze.is(neighbour.pos, '#') && !visited(neighbour.pos -> neighbour.dir)
            .foldLeft(acc.queue): (queue, reindeer) =>
              queue.updatedWith(reindeer.cost):
                case Some(reindeers) => Some(reindeers :+ reindeer)
                case None            => Some(Vector(reindeer))
        )

    def solution1: Option[Long] =
      headValues.find(_.pos == end).map(_.cost)

    def solution2: Option[Long] =
      Option.when(headValues.exists(_.pos == end)):
        headValues.filter(_.pos == end).flatMap(_.path).distinct.size

  object ReindeerMaze:
    def apply(lines: Vector[String]): ReindeerMaze =
      val (start, end) = (lines.loc('S'), lines.loc('E'))
      new ReindeerMaze(lines, end, Set.empty, TreeMap(0L -> Vector(Reindeer(0, start, Dir.E, Vector(start)))))

end Day16
