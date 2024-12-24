package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day24 extends AoC:
  def part1(lines: Vector[String]): Long =
    Iterator.iterate(parse(lines))(_.nextState).findMap(_.solution)
  end part1

  def part2(lines: Vector[String]): Long =
    1
  end part2

  case class Machine(inputs: Map[String, Long], zs: Vector[String], gates: Vector[Gate]):
    def nextState: Machine =
      val nexts = for
        ((i0, i1), op, o) <- gates
        v0                <- inputs.get(i0)
        v1                <- inputs.get(i1)
      yield o -> op.apply(v0, v1)
      copy(inputs = inputs ++ nexts)

    def solution: Option[Long] =
      Option.when(zs.forall(inputs.contains)):
        zs.foldRight(0L):
          case (o, acc) => (acc << 1) + inputs(o)

  enum Op:
    case AND, XOR, OR

    def apply(i0: Long, i1: Long): Long = this match
      case AND => i0 & i1
      case XOR => i0 ^ i1
      case OR  => i0 | i1

  object Op:
    def unapply(s: String): Option[Op] = Some(Op.valueOf(s))

  type Gate = ((String, String), Op, String)

  def parse(lines: Vector[String]): Machine =
    val (inputsStr, gatesStr) = lines.span(_.nonEmpty)
    val inputs                = inputsStr.foldCollect:
      case s"$input: ${L(value)}" => Map(input -> value)
    val gates                 = gatesStr.collect:
      case s"$i0 ${Op(op)} $i1 -> $o" => ((i0, i1), op, o)
    Machine(inputs, gates.map(_._3).filter(_.startsWith("z")).sorted, gates)

end Day24
