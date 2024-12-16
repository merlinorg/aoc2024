package org.merlin.aoc2024

import org.openjdk.jmh.annotations.Benchmark

class AoCBench:
  @Benchmark
  def day11_1(): Unit = Day11.part1(Day11.lines1)

  @Benchmark
  def day11_2(): Unit = Day11.part2(Day11.lines2)

  @Benchmark
  def day16_1(): Unit = Day16.part1(Day16.lines1)

  @Benchmark
  def day16_2(): Unit = Day16.part2(Day16.lines2)
end AoCBench
