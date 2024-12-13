package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day12 extends AoC:

  override def part1(board: Board): Long =
    parse(board).foldMap: region =>
      region.area * region.perimeter
  end part1

  override def part2(board: Board): Long =
    parse(board).foldMap: region =>
      region.area * region.sides
  end part2

  type Region = Set[Loc]

  private def parse(board: Board): Vector[Region] =
    board.locations.foldLeft(Vector.empty[Region]): (regions, loc) =>
      if regions.exists(_.contains(loc)) then regions
      else regions :+ floodfill(board, loc, Set.empty)

  private def floodfill(board: Board, loc: Loc, region: Region): Region =
    loc.adjacents.foldLeft(region + loc): (region, adj) =>
      if region(adj) || !board.is(adj, loc) then region
      else floodfill(board, adj, region)

  extension (region: Region)
    def area: Long      = region.size
    def perimeter: Long = region.foldMap(_.adjacents.count(adj => !region(adj)))
    def sides: Long     = region.foldMap: loc =>
      CardinalDirections.count: dir =>
        !region(loc + dir) && (!region(loc + dir.ccw) || region(loc + dir.ccw2))

end Day12
