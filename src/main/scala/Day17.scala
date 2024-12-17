package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

object Day17 extends AoC:
  def part1(lines: Vector[String]): String =
    val (cpu, program) = parse(lines)
    execute(cpu, program).mkString(",")
  end part1

  // Observe that the core of the program is:
  //   a = a >> 3 ... jnz 0
  // Hypostulate that we just need to twiddle octal triads from the top down.
  override def part2(lines: Vector[String]): Long =
    val (cpu, program) = parse(lines)

    def solve(a: Long): Option[Long] =
      val out = execute(cpu.copy(a = a), program)
      Option
        .when(out == program)(a)
        .orElse(program.endsWith(out).flatOption(solve(a << 3)))
        .orElse((a % 8 < 7).flatOption(solve(a + 1)))

    solve(1L).get
  end part2

  private def execute(cpu: CPU, program: Vector[Int]): Vector[Int] =
    Iterator
      .iterate(cpu): cpu =>
        step(cpu, program)
      .findMap: cpu =>
        Option.when(cpu.halt)(cpu.out)

  case class CPU(pc: Int, a: Long, b: Long, c: Long, out: Vector[Int], halt: Boolean)

  object CPU:
    val itanium: CPU = CPU(0, 0, 0, 0, Vector.empty, false)

  enum Instruction:
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

  private def step(cpu: CPU, program: Vector[Int]): CPU =
    import Instruction.*
    if cpu.pc < program.length - 1 then
      (fromOrdinal(program(cpu.pc)), program(cpu.pc + 1)) match
        case (Adv, operand) =>
          cpu.copy(pc = cpu.pc + 2, a = cpu.a >> combo(cpu, operand))
        case (Bxl, operand) =>
          cpu.copy(pc = cpu.pc + 2, b = cpu.b ^ operand)
        case (Bst, operand) =>
          cpu.copy(pc = cpu.pc + 2, b = combo(cpu, operand) % 8)
        case (Jnz, operand) =>
          cpu.copy(pc = if cpu.a == 0 then cpu.pc + 2 else operand)
        case (Bxc, _)       =>
          cpu.copy(pc = cpu.pc + 2, b = cpu.b ^ cpu.c)
        case (Out, operand) =>
          cpu.copy(pc = cpu.pc + 2, out = cpu.out :+ (combo(cpu, operand) % 8).toInt)
        case (Bdv, operand) =>
          cpu.copy(pc = cpu.pc + 2, b = cpu.a >> combo(cpu, operand))
        case (Cdv, operand) =>
          cpu.copy(pc = cpu.pc + 2, c = cpu.a >> combo(cpu, operand))
    else cpu.copy(halt = true)

  private def combo(cmp: CPU, op: Long): Long = op match
    case 0 | 1 | 2 | 3 => op
    case 4             => cmp.a
    case 5             => cmp.b
    case 6             => cmp.c
    case _             => ???

  def parse(lines: Vector[String]): (CPU, Vector[Int]) =
    val (registers, programmes) = lines.span(_.nonEmpty)

    val cpu = registers.foldLeft(CPU.itanium):
      case (cpu, s"Register A: ${I(a)}") => cpu.copy(a = a)
      case (cpu, s"Register B: ${I(b)}") => cpu.copy(b = b)
      case (cpu, s"Register C: ${I(c)}") => cpu.copy(c = c)
      case (cpu, _)                      => cpu

    (cpu, programmes(1).numbers.map(_.toInt))

  object I:
    def unapply(s: String): Option[Long] = s.toLongOption

end Day17
