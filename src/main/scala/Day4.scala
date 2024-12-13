package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day4 extends AoC:

  override def part1(board: Board): Long =
    board.locations.foldMap: loc =>
      Dir.values.count: dir =>
        "XMAS".zipWithIndex.forall:
          case (c, index) =>
            board.is(loc + dir * index, c)
  end part1

  override def part2(board: Board): Long =
    board.locations.count: loc =>
      board.is(loc, 'A') &&
        2 == OrdinalDirections.count: dir =>
          board.is(loc + dir, 'M') && board.is(loc + dir.reverse, 'S')
  end part2

end Day4
