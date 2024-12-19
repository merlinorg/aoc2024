package org.merlin.aoc2024

import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable

import scalaz.*
import Scalaz.*

object Day6 extends AoC:

  override def part1(board: Board): Long =
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

  type LocDir = (Loc, Dir)

  override def part2(board: Board): Long =
    val cache = mutable.Map.empty[LocDir, (Boolean, Set[Loc])]
    val crud = Iterator.iterate(Guard1FSM(board))(_.next).find(_.solution.isDefined).get.visited
    board.locations.count: obstacle =>
      if !board.is(obstacle, '.') || !crud(obstacle) then false
      else
        val res    = Iterator
          .iterate(Guard2FSM(board, obstacle))(_.next)
          .find: guard =>
            cache.get(guard.locdir) match
              case Some(_, visited) if !visited.contains(obstacle) => true
              case _ => guard.solution.isDefined
          .get
        if res.solution.isDefined then
          val looped = res.looped
          val locs = res.pure.map(_._1).toSet
          if !looped || res.pure.contains(res.locdir) then
            res.pure.tails.foreach: locdirv =>
              if locdirv.nonEmpty then
                val locdir = locdirv.head
                if looped then cache.update(locdir, looped -> locs)
                else cache.update(locdir, looped -> locdirv.map(_._1).toSet)
          looped
        else
          cache(res.locdir)._1

  end part2

  case class Guard2FSM(
    loc: Loc,
    dir: Dir,
    board: Board,
    obstacle: Loc,
    visited: Set[(Loc, Dir)],
    pure: Vector[(Loc, Dir)]
  ):
    val locdir = loc -> dir
    val looped = visited.contains(locdir)

    def solution: Option[Boolean] =
      (looped || (loc <>= board)).option(looped)

    def next: Guard2FSM =
      val nxt = loc + dir
      if (board.is(nxt, '#') || nxt == obstacle)
        copy(
          dir = dir.cw,
          visited = visited + locdir,
          pure = if nxt == obstacle then Vector.empty else pure :+ locdir
        )
      else
        copy(loc = nxt, visited = visited + locdir, pure = pure :+ locdir)

  object Guard2FSM:
    def apply(board: Board, obstacle: Loc): Guard2FSM =
      Guard2FSM(board.find('^'), Dir.N, board, obstacle, Set.empty, Vector.empty)

end Day6
