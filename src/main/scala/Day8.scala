package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day8 extends AoC:

  def part1(board: Board): Long =
    parse(board)
      .flatMap: locations =>
        locations.allPairs.flatMap: (a, b) =>
          Vector(a + (a - b), b + (b - a)).filter(_ >=< board)
      .toSet
      .size
  end part1

  def part2(board: Board): Long =
    parse(board)
      .flatMap: locations =>
        locations.allPairs.flatMap: (a, b) =>
          Iterator.iterate(a)(_ + (a - b)).takeWhile(_ >=< board) ++
            Iterator.iterate(b)(_ + (b - a)).takeWhile(_ >=< board)
      .toSet
      .size
  end part2

  private def parse(board: Board): Iterable[Vector[Loc]] =
    board.locations
      .filter(loc => Character.isLetterOrDigit(board(loc)))
      .groupMap(board(_))(identity)
      .values

end Day8
