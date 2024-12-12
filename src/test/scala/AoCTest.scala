package org.merlin.aoc2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AoCTest extends AnyFreeSpec with Matchers {
  test(Day1, 11, 765748, 31, 27732508)
  test(Day2, 2, 591, 4, 621)
  test(Day3, 161, 178538786, 48, 102467299)
  test(Day4, 18, 2447, 9, 1868)
  test(Day5, 143, 4774, 123, 6004)
  test(Day6, 41, 4967, 6, 1789)
  test(Day7, 3749, 2664460013123L, 11387, 426214131924213L)
  test(Day8, 14, 371, 34, 1229)
  test(Day9, 1928, 6399153661894L, 2858, 6421724645083L)
  test(Day10, 36, 733, 81, 1514)
  test(Day11, 55312, 197157, 65601038650482L, 234430066982597L)
  test(Day12, 1930, 1381056, 1206, 834828)

  def test(day: AoC, sample1: Long, answer1: Long, sample2: Long, answer2: Long): Unit = {
    val name = day.getClass.getSimpleName.dropRight(1)
    name - {
      "Part 1" - {
        "Sample Input" in {
          day.part1(day.readLines(1, sample = true)) shouldBe sample1
        }
        "Real Input" in {
          day.part1(day.readLines(1, sample = false)) shouldBe answer1
        }
      }
      "Part 2" - {
        "Sample Input" in {
          day.part2(day.readLines(2, sample = true)) shouldBe sample2
        }
        "Real Input" in {
          day.part2(day.readLines(2, sample = false)) shouldBe answer2
        }
      }
    }
  }
}
