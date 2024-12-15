package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day15 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (start, map, directions) = parse(lines)

    val (_, result) = directions.foldLeft(start -> map):
      case ((loc, map), dir) =>
        val next = loc + dir
        val end  = Iterator.iterate(next)(_ + dir).dropWhile(loc => map.get(loc).contains('O')).next
        if map.contains(end) then (loc, map)
        else (next, map + (end -> 'O') - next)

    result.toVector.foldMap: (loc, c) =>
      if c == 'O' then loc.y * 100 + loc.x else 0
  end part1

  override def part2(lines: Vector[String]): Long =
    val (start, map, directions) = parse(lines)
    val expanded                 = map.flatMap:
      case (loc, 'O') => Seq(Loc(loc.x * 2, loc.y) -> '[', Loc(loc.x * 2 + 1, loc.y) -> ']')
      case (loc, c)   => Seq(Loc(loc.x * 2, loc.y) -> c, Loc(loc.x * 2 + 1, loc.y) -> c)

    val (_, result) = directions.foldLeft(Loc(start.x * 2, start.y) -> expanded):
      case ((loc, map), dir) =>
        val wavefronts = Iterator
          .iterate(Set(loc + dir)): wavefront =>
            wavefront.flatMap: loc =>
              map.get(loc) match
                case Some('[') if !dir.horizontal => Seq(loc + dir, loc + dir + Dir.E)
                case Some(']') if !dir.horizontal => Seq(loc + dir, loc + dir + Dir.W)
                case Some('#')                    => Seq(loc)
                case Some(_)                      => Seq(loc + dir)
                case _                            => Seq.empty
          .takeWhile: wavefront =>
            wavefront.nonEmpty && !wavefront.exists(map.get(_).contains('#'))
          .toVector
        if wavefronts.lastOption.exists(_.forall(!map.contains(_))) then
          loc + dir -> wavefronts.tail.flatten.foldRight(map): (loc, acc) =>
            val prev = loc + dir.reverse
            acc + (loc -> map(prev)) - prev
        else (loc, map)

    result.toVector.foldMap: (loc, c) =>
      if c == '[' then loc.y * 100 + loc.x else 0
  end part2

  private val Directions = Map('<' -> Dir.W, '^' -> Dir.N, '>' -> Dir.E, 'v' -> Dir.S)

  private def parse(lines: Vector[String]): (Loc, Map[Loc, Char], Seq[Dir]) =
    val (maps, dirs) = lines.span(_.nonEmpty)
    val tuples       = for
      (row, y)  <- maps.zipWithIndex
      (char, x) <- row.zipWithIndex
      if char == 'O' || char == '#'
    yield Loc(x, y) -> char
    (maps.find('@'), tuples.toMap, dirs.mkString.map(Directions))

end Day15
