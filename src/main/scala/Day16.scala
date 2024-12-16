package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day16 extends AoC:
  case class Reindeer(cost: Long, pos: Loc, dir: Dir, path: Set[Loc]):
    def nextReindeers: List[Reindeer] = List(
      Reindeer(cost + 1, pos + dir, dir, path + (pos + dir)),
      Reindeer(cost + 1000, pos, dir.cw, path),
      Reindeer(cost + 1000, pos, dir.ccw, path)
    )

  def part1(lines: Vector[String]): Long = Iterator.iterate(Part1BFS(lines))(_.nextState).findMap(_.solution)

  case class Part1BFS(
    lines: Vector[String],
    costs: Map[(Loc, Dir), Long],
    reindeers: Vector[Reindeer],
  ):
    def nextState: Part1BFS =
      val (nextCosts, nextReindeers) = reindeers.foldLeft((costs, Vector.empty[Reindeer])):
        case ((costs, reindeers), reindeer) =>
          if !lines.is(reindeer.pos, '#') && costs.get(reindeer.pos -> reindeer.dir).forall(_ > reindeer.cost) then
            (costs + (reindeer.pos -> reindeer.dir -> reindeer.cost), reindeers ++ reindeer.nextReindeers)
          else (costs, reindeers)
      Part1BFS(lines, nextCosts, nextReindeers)

    def solution: Option[Long] =
      Option.when(reindeers.isEmpty)(cost)

    def cost: Long =
      val end    = lines.find('E')
      val result = for
        dir  <- CardinalDirections
        cost <- costs.get(end -> dir)
      yield cost
      result.min

  object Part1BFS:
    def apply(lines: Vector[String]): Part1BFS =
      val start = lines.find('S')
      new Part1BFS(lines, Map.empty, Vector(Reindeer(0, start, Dir.E, Set(start))))

  override def part2(lines: Vector[String]): Long = Iterator.iterate(Part2BFS(lines))(_.nextState).findMap(_.solution)

  case class Part2BFS(
    lines: Vector[String],
    costs: Map[(Loc, Dir), Long],
    reindeers: Vector[Reindeer],
    bests: (Long, Set[Loc]),
  ):
    private val end = lines.find('E')

    def nextState: Part2BFS =
      val (nextCosts, nextBests, nextReindeers) = reindeers.foldLeft((costs, bests, Vector.empty[Reindeer])):
        case ((costs, bests, reindeers), reindeer) =>
          val nextBests =
            if reindeer.pos != end || reindeer.cost > bests(0) then bests
            else if reindeer.cost < bests(0) then reindeer.cost -> reindeer.path
            else bests(0)                                       -> (bests(1) ++ reindeer.path)

          val nextStates = reindeer.nextReindeers.filter: next =>
            !lines.is(next.pos, '#') && costs.get(next.pos -> next.dir).forall(_ >= next.cost)

          val nextCosts = costs ++ nextStates.map(next => next.pos -> next.dir -> next.cost)

          (nextCosts, nextBests, reindeers ++ nextStates)

      Part2BFS(lines, nextCosts, nextReindeers, nextBests)

    def solution: Option[Long] =
      Option.when(reindeers.isEmpty)(bests(1).size)

  object Part2BFS:
    def apply(lines: Vector[String]): Part2BFS =
      val start = lines.find('S')
      new Part2BFS(
        lines,
        Map(start -> Dir.E -> 0),
        Vector(Reindeer(0, start, Dir.E, Set(start))),
        Long.MaxValue -> Set.empty
      )

end Day16
