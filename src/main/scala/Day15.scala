package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day15 extends AoC:
  def part1(lines: Vector[String]): Long =
    val (map, dirs) = lines.span(_.nonEmpty)
    solve(map, dirs)
  end part1

  def part2(lines: Vector[String]): Long =
    val (map, dirs) = lines.span(_.nonEmpty)
    val expanded    = map.map: row =>
      row.map(c => if c == 'O' then "[]" else s"$c$c").mkString
    solve(expanded, dirs)
  end part2

  private val Directions = Map('<' -> Dir.W, '^' -> Dir.N, '>' -> Dir.E, 'v' -> Dir.S)

  private def solve(map: Vector[String], dirs: Vector[String]): Long =
    val (_, result) = dirs
      .flatMap(_.map(Directions))
      .foldLeft(map.loc('@') -> map.map(_.replace('@', '.'))):
        case ((loc, map0), dir) =>
          Iterator
            .iterate(Set(loc + dir) -> map0): (in, map) =>
              val out     = in.filterNot(map.is(_, '.')) ++
                (if dir.horizontal then Set.empty
                 else in.filter(map.is(_, '[')).map(_ + Dir.E) ++ in.filter(map.is(_, ']')).map(_ + Dir.W))
              val cleared = out.foldLeft(map)((map, loc) => map.update(loc, '.'))
              out.map(_ + dir) -> in.foldLeft(cleared)((map, loc) => map.update(loc, map0(loc - dir)))
            .findCollect:
              case (locs, map) if locs.isEmpty              => loc + dir -> map
              case (locs, _) if locs.exists(map.is(_, '#')) => loc       -> map0

    result.locations.foldMap: loc =>
      if result.is(loc, 'O') || result.is(loc, '[') then loc.y * 100 + loc.x else 0

end Day15
