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
      else regions :+ floodfill(board, board(loc), loc, Set.empty)

  private def floodfill(board: Board, char: Char, loc: Loc, region: Region): Region =
    loc.adjacents.foldLeft(region + loc): (region, adj) =>
      if region(adj) || !board.is(adj, char) then region
      else floodfill(board, char, adj, region)

  extension (region: Region)
    def area: Long      = region.size
    def perimeter: Long = region.foldMap(_.adjacents.count(adj => !region(adj)))
    def sides: Long     = region.foldMap: loc =>
      Dir.cardinalValues.count: dir =>
        !region(loc + dir) && (!region(loc + dir.ccw) || region(loc + dir.ccw2))

end Day12
