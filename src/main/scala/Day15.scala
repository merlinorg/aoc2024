package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day15 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (map, dirs) = lines.span(_.nonEmpty)
    solve(map, dirs)
  end part1

  override def part2(lines: Vector[String]): Long =
    val (map, dirs) = lines.span(_.nonEmpty)
    val expanded    = map.map: row =>
      row.map(c => if c == 'O' then "[]" else s"$c$c").mkString
    solve(expanded, dirs)
  end part2

  private val Directions = Map('<' -> Dir.W, '^' -> Dir.N, '>' -> Dir.E, 'v' -> Dir.S)

  private def solve(map: Vector[String], dirs: Vector[String]): Long =
    val (_, result) = dirs
      .flatMap(_.map(Directions))
      .foldLeft(map.find('@') -> map):
        case ((loc, map), dir) =>
          val wavefronts = Iterator
            .iterate(Set(loc + dir)): wavefront =>
              wavefront.flatMap: loc =>
                map(loc) match
                  case '[' if dir.vertical => Seq(loc + dir, loc + dir + Dir.E)
                  case ']' if dir.vertical => Seq(loc + dir, loc + dir + Dir.W)
                  case 'O' | '[' | ']'     => Seq(loc + dir)
                  case _                   => Seq.empty
            .takeUntil: wavefront =>
              wavefront.isEmpty || wavefront.exists(map.is(_, '#'))
            .toVector
          if wavefronts.last.isEmpty then
            loc + dir -> wavefronts.flatten.foldRight(map): (loc, acc) =>
              val prev = loc + dir.reverse
              acc.update(loc, map(prev)).update(prev, '.')
          else (loc, map)

    result.locations.foldMap: loc =>
      if result.is(loc, 'O') || result.is(loc, '[') then loc.y * 100 + loc.x else 0

end Day15
