package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.collection.mutable

object Day21 extends AoC:
  def part1(lines: Vector[String]): Long = solve(lines, 2)

  def part2(lines: Vector[String]): Long = solve(lines, 25)

  def solve(lines: Vector[String], robots: Int): Long =
    val cache = mutable.Map.empty[(Loc, Loc, Int), Long]

    def shortestMove(src: Loc, dst: Loc, stage: Int): Long = cache.memo((src, dst, stage)):
      val pad     = if stage == 0 then Keypad else Dirpad
      val answers = bfs((src, Vector.empty[Char])):
        case (loc, keys) if loc == dst =>
          Right(if stage < robots then shortedSolution(keys :+ 'A', stage + 1) else keys.length + 1L)
        case (loc, keys)               =>
          Left((loc +-> dst).filterNot(dir => pad.is(loc + dir, ' ')).map(dir => (loc + dir, keys :+ DirKeys(dir))))
      answers.min

    def shortedSolution(sequence: Vector[Char], stage: Int): Long =
      val pad = if stage == 0 then Keypad else Dirpad
      sequence.foldLeftMap(pad.find('A') -> 0L)(_._2):
        case ((src, total), char) =>
          val dst = pad.find(char)
          dst -> (total + shortestMove(src, dst, stage))

    lines.foldMap: line =>
      shortedSolution(line.toVector, 0) * line.numbers.head

  private val Keypad  = Vector("789", "456", "123", " 0A")
  private val Dirpad  = Vector(" ^A", "<v>")
  private val DirKeys = Map(Dir.E -> '>', Dir.W -> '<', Dir.N -> '^', Dir.S -> 'v')

end Day21
