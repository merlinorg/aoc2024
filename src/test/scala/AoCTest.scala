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
  test(Day13, 480, 36954, 875318608908L, 79352015273424L)
  test(Day14, 12, 236628054, 0, 7584)
  test(Day15, 10092, 1465523, 9021, 1471049)
  test(Day16, 7036, 88468, 45, 616)
  test(Day16Alt, 7036, 88468, 45, 616)
  test(Day16Mut, 7036, 88468, 45, 616)
  test(Day17, "4,6,3,5,6,3,5,2,1,0", "6,2,7,2,3,1,6,0,5", 117440, 236548287712877L)
  test(Day18, 22, 302, Loc(6, 1), Loc(24, 32))
  test(Day19, 6, 267, 16, 796449099271652L)
  test(Day19Mut, 6, 267, 16, 796449099271652L)
  test(Day20, 1, 1263, 285, 957831)
  test(Day21, 126384, 179444, 154115708116294L, 223285811665866L)
  test(Day22, 37327623, 13185239446L, 23, 1501)

  def test(day: AoC, sample1: Any, answer1: Any, sample2: Any, answer2: Any): Unit = {
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
