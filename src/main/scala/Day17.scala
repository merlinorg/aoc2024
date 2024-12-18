package org.merlin.aoc2024

import scalaz.*
import scalaz.Scalaz.*

import scala.annotation.tailrec

object Day17 extends AoC:
  def part1(lines: Vector[String]): String =
    execute(parse(lines)).mkString(",")
  end part1

  // Observe that the core of the program is ... a = a >> 3 ... jnz 0
  // Hypostulate that we just need to twiddle octal triads from the top down.
  override def part2(lines: Vector[String]): Long =
    val cpu = parse(lines)
    Iterator
      .iterate(1L): a =>
        if cpu.program.endsWith(execute(cpu.copy(a = a))) then a << 3 else (a + 1).dropz
      .findMap: a =>
        Option.when(execute(cpu.copy(a = a)) == cpu.program)(a)
  end part2

  private def execute(cpu: CPU): Vector[Int] =
    Iterator.iterate(cpu)(_.step).findMap(_.output)

  case class CPU(pc: Int, a: Long, b: Long, c: Long, program: Vector[Int], out: Vector[Int]):
    import Instruction.*

    def step: CPU = (fromOrdinal(program(pc)), program(pc + 1)) match
      case (Adv, operand) => copy(pc = pc + 2, a = a >> combo(operand))
      case (Bxl, operand) => copy(pc = pc + 2, b = b ^ operand)
      case (Bst, operand) => copy(pc = pc + 2, b = combo(operand) % 8)
      case (Jnz, operand) => copy(pc = if a == 0 then pc + 2 else operand)
      case (Bxc, _)       => copy(pc = pc + 2, b = b ^ c)
      case (Out, operand) => copy(pc = pc + 2, out = out :+ (combo(operand) % 8).toInt)
      case (Bdv, operand) => copy(pc = pc + 2, b = a >> combo(operand))
      case (Cdv, operand) => copy(pc = pc + 2, c = a >> combo(operand))

    def output: Option[Vector[Int]] = Option.when(pc >= program.length)(out)

    private def combo(op: Long): Long =
      if op == 4 then a else if op == 5 then b else if op == 6 then c else op

  enum Instruction:
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

  def parse(lines: Vector[String]): CPU =
    lines.foldLeft(CPU(0, 0, 0, 0, Vector.empty, Vector.empty)):
      case (cpu, s"Register A: ${L(a)}") => cpu.copy(a = a)
      case (cpu, s"Register B: ${L(b)}") => cpu.copy(b = b)
      case (cpu, s"Register C: ${L(c)}") => cpu.copy(c = c)
      case (cpu, s"Program: ${p}")       => cpu.copy(program = p.numbers.map(_.toInt))
      case (cpu, _)                      => cpu

  extension (n: Long) @tailrec def dropz: Long = if n % 8 != 0 then n else (n >> 3).dropz

end Day17
