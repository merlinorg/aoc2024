package org.merlin.aoc2024

import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

private val NumRe: Regex = "-?\\d+".r

trait AoC:
  def main(args: Array[String]): Unit =
    val part   = if (args.contains("2")) 2 else 1
    val sample = args.contains("sample")

    val lines  = readLines(part, sample)
    val result = if (part == 1) part1(lines) else part2(lines)

    println(result)
  end main

  def part1(lines: Vector[String]): Any

  def part2(lines: Vector[String]): Any

  val lines1: Vector[String] = readLines(1, false)

  val lines2: Vector[String] = readLines(2, false)

  def readLines(part: Int, sample: Boolean): Vector[String] =
    val day = NumRe.findFirstIn(getClass.getSimpleName).get
    val source = Try:
      Source.fromResource(if (sample) s"day-$day-sample-$part.txt" else s"day-$day-$part.txt")
    .getOrElse:
      Source.fromResource(if (sample) s"day-$day-sample.txt" else s"day-$day.txt")
    Using.resource(source)(_.getLines.toVector)
  end readLines
end AoC
