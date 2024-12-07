package org.merlin.aoc2024

import scalaz.*
import Scalaz.*

object Day4 extends AoC:

  override def part1(board: Board): Long =
    board.locations.foldMap: loc =>
      Dir.values.count: dir =>
        "XMAS".zipWithIndex.forall:
          case (c, index) =>
            board.get(loc + dir * index).contains(c)
  end part1

  override def part2(board: Board): Long =
    board.locations.count: loc =>
      board.get(loc).contains('A') &&
        2 == Dir.diagonalValues.count: dir =>
          board.get(loc + dir).contains('M') && board.get(loc + dir.reverse).contains('S')
  end part2

end Day4
