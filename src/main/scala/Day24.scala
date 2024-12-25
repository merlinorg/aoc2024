package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day24 extends AoC:
  def part1(lines: Vector[String]): Long = parse(lines).z

  def part2(lines: Vector[String]): String =
    if lines.length == 47 then "" else solve(parse(lines)).flatten.toVector.sorted.mkString(",")

  private def solve(machine: Machine): Iterator[Vector[String]] =
    Iterator.unfold(machine): machine =>
      Option.when(machine.broken)(fix(machine).next())

  private def fix(machine: Machine): Iterator[(Vector[String], Machine)] =
    for
      key     <- machine.keys("z").iterator // all outputs
      index    = key.tail.toInt             // output bit index
      circuit  = bitAdd(index)              // correct full adder circuit
      if machine.circuit(key) != circuit    // if the machine equation is incorrect
      key0    <- machine.inGates(key)       // for all keys that feed this output
      key1    <- machine.gates.keySet       // for all other keys
      machine2 = machine.swap(key0, key1)   // swap the gates
      if machine2.circuit(key) == circuit   // find the fix
    yield Vector(key0, key1) -> machine2    // profit

  def bitAdd(i: Int): String =
    val (x, y) = (pad("x", i), pad("y", i))
    if i == 0 then Op.XOR(x, y) else Op.XOR(Op.XOR(x, y), bitOverflow(i - 1))

  def bitOverflow(i: Int): String =
    val (x, y) = (pad("x", i), pad("y", i))
    if i == 0 then Op.AND(x, y) else Op.OR(Op.AND(Op.XOR(x, y), bitOverflow(i - 1)), Op.AND(x, y))

  case class Machine(gates: Map[String, Gate], inputs: Map[String, Long]):
    def swap(output0: String, output1: String): Machine =
      val (gate0, gate1) = (gates(output0), gates(output1))
      copy(gates = gates + (output0 -> gate1) + (output1 -> gate0))

    def z: Long = binary("z")

    def broken: Boolean = binary("x") + binary("y") != z

    def binary(prefix: String): Long =
      keys(prefix).foldRight(0L):
        case (key, acc) => (acc << 1) + solve(key, Set.empty)

    def keys(prefix: String): Vector[String] =
      (inputs.keySet ++ gates.keySet).filter(_.startsWith(prefix)).toVector.sorted

    def solve(key: String, loop: Set[String]): Long =
      if loop(key) then -1
      else
        gates.get(key) match
          case Some((i0, i1), op, _) => op(solve(i0, loop + key), solve(i1, loop + key))
          case None                  => inputs.getOrElse(key, -1L)

    def circuit(key: String, loop: Set[String] = Set.empty): String =
      if loop(key) then ""
      else
        gates.get(key) match
          case Some((i0, i1), op, _) => op(circuit(i0, loop + key), circuit(i1, loop + key))
          case None                  => key

    def inGates(key: String): Set[String] = gates.get(key) match
      case Some((i0, i1), _, _) => Set(key) ++ inGates(i0) ++ inGates(i1)
      case None                 => Set.empty

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
        case s"$input: ${L(value)}" => input -> value
    )
end Day24
