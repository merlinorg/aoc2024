package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day24 extends AoC:
  def part1(lines: Vector[String]): Long = parse(lines).solution._3

  def part2(lines: Vector[String]): String =
    if lines.length == 47 then "" else solve(parse(lines), Vector.empty)

  private def solve(machine: Machine, swaps: Vector[String]): String =
    val (x, y, z) = machine.solution
    if x + y == z then
      swaps.sorted.mkString(",")
    else
      val solver = for
        key <- machine.keys("z").iterator
        index = key.tail.toInt
        correct = add2bit(index)
        if machine.circuit(key) != correct
        o0 <- machine.inGates(key)
        o1 <- machine.gates.keySet
        machine2 = machine.swap(o0, o1)
        if machine2.circuit(key) == correct
      yield machine2 -> Vector(o0, o1)
      val (machine2, swap) = solver.next()
      solve(machine2, swaps ++ swap)
  end solve

  def add2bit(i: Int): String =
    val (x, y) = (pad("x", i), pad("y", i))
    if i == 0 then Op.XOR(x, y) else Op.XOR(Op.XOR(x, y), overflow2bit(i - 1))

  def overflow2bit(i: Int): String =
    val (x, y) = (pad("x", i), pad("y", i))
    if i == 0 then Op.AND(x, y) else Op.OR(Op.AND(Op.XOR(x, y), overflow2bit(i - 1)), Op.AND(x, y))

  case class Machine(gates: Map[String, Gate], inputs: Map[String, Long]):
    def swap(output0: String, output1: String): Machine =
      val (gate0, gate1) = (gates(output0), gates(output1))
      copy(gates = gates + (output0 -> gate1) + (output1 -> gate0))

    def solution: (Long, Long, Long) = (binary("x"), binary("y"), binary("z"))

    def binary(prefix: String): Long =
      keys(prefix).foldRight(0L):
        case (key, acc) => (acc << 1) + solve(key, Set.empty)

    def keys(prefix: String): Vector[String] =
      (inputs.keySet ++ gates.keySet).filter(_.startsWith(prefix)).toVector.sorted

    def solve(key: String, loop: Set[String]): Long =
      if loop(key) then -1
      else gates.get(key) match
        case Some((i0, i1), op, _) => op(solve(i0, loop + key), solve(i1, loop + key))
        case None                  => inputs.getOrElse(key, -1L)

    def circuit(key: String, loop: Set[String] = Set.empty): String =
      if loop(key) then ""
      else gates.get(key) match
        case Some((i0, i1), op, _) => op(circuit(i0, loop + key), circuit(i1, loop + key))
        case None => key

    def inGates(key: String): Set[String] = gates.get(key) match
      case Some((i0, i1), _, _) => Set(key) ++ inGates(i0) ++ inGates(i1)
      case None => Set.empty


  def pad(prefix: String, i: Long) = if i < 10 then s"${prefix}0$i" else s"$prefix$i"

  enum Op:
    case AND, XOR, OR

    def apply(x0: Long, x1: Long): Long = this match
      case AND => x0 & x1
      case XOR => x0 ^ x1
      case OR  => x0 | x1

    def apply(s0: String, s1: String): String = Vector(s0, s1).sorted.mkString("(", s" $this ", ")")

  object Op:
    def unapply(s: String): Option[Op] = Some(Op.valueOf(s))

  type Gate = ((String, String), Op, String)

  def parse(lines: Vector[String]): Machine =
    val (inputsStr, gatesStr) = lines.span(_.nonEmpty)
    Machine(
      gatesStr.collectToMap:
        case s"$i0 ${Op(op)} $i1 -> $o" => o -> ((i0, i1), op, o),
      inputsStr.collectToMap:
        case s"$input: ${L(value)}" => input -> value,
    )
end Day24
