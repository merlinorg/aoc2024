package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*
import scala.collection.mutable

object Day8 extends AoC:

  override def part1(board: Board): Long =
    parse(board)
      .flatMap: locations =>
        locations.allPairs.flatMap: (a, b) =>
          Vector(a + (a - b), b + (b - a))
      .filter: loc =>
        loc >=< board
      .toSet
      .size
  end part1

  override def part2(board: Board): Long =
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
