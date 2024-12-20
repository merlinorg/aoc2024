package org.merlin.aoc2024

import scala.collection.parallel.CollectionConverters.*

import scalaz.*
import Scalaz.*

object Day6 extends AoC:

  def part1(board: Board): Long =
    Iterator.iterate(Guard1FSM(board))(_.next).findMap(_.solution)
  end part1

  case class Guard1FSM(loc: Loc, dir: Dir, board: Board, visited: Set[Loc]):
    def solution: Option[Long] = (loc <>= board).option(visited.size)

    def next: Guard1FSM =
      val nxt = loc + dir
      if (board.is(nxt, '#'))
        copy(dir = dir.cw)
      else
        copy(loc = nxt, visited = visited + loc)

  object Guard1FSM:
    def apply(board: Board): Guard1FSM =
      Guard1FSM(board.find('^'), Dir.N, board, Set.empty)

  def part2(board: Board): Long =
    board.locations.par.count: obstacle =>
      board.is(obstacle, '.') && Iterator.iterate(Guard2FSM(board, obstacle))(_.next).findMap(_.solution)
  end part2

  case class Guard2FSM(loc: Loc, dir: Dir, board: Board, obstacle: Loc, visited: Set[(Loc, Dir)]):
    def solution: Option[Boolean] =
      val looped = visited.contains(loc -> dir)
      (looped || (loc <>= board)).option(looped)

    def next: Guard2FSM =
      val nxt = loc + dir
      if (board.is(nxt, '#') || nxt == obstacle)
        copy(dir = dir.cw, visited = visited + (loc -> dir))
      else
        copy(loc = nxt, visited = visited + (loc -> dir))

  object Guard2FSM:
    def apply(board: Board, obstacle: Loc): Guard2FSM =
      Guard2FSM(board.find('^'), Dir.N, board, obstacle, Set.empty)

end Day6
