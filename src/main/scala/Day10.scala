package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day10 extends AoC:

  def part1(board: Board): Long =
    trails(board).foldMap(_.toSet.size)
  end part1

  def part2(board: Board): Long =
    trails(board).foldMap(_.size)
  end part2

  private def trails(board: Board): Vector[Vector[Loc]] =
    board
      .findAll('0')
      .map: loc =>
        Iterator
          .iterate('0'.toInt -> Vector(loc)):
            case (digit, locations) =>
              (digit + 1, locations.flatMap(_.adjacents.filter(board.is(_, digit + 1))))
          .findMap:
            case (digit, locations) =>
              Option.when(digit == '9')(locations)

end Day10
