package org.merlin.aoc2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AoCTest extends AnyFreeSpec with Matchers {
  test(Day1, 11, 765748, 31, 27732508)
  test(Day2, 2, 591, 4, 621)
  test(Day3, 161, 178538786, 48, 102467299)

  def test(day: AoC, sample1: Long, answer1: Long, sample2: Long, answer2: Long): Unit = {
    val name = day.getClass.getSimpleName.dropRight(1)
    val num  = "\\d+".r.findFirstIn(name).get.toInt
    name - {
      "Part 1" - {
        "Sample Input" in {
          day.part1(readLines(num, 1, sample = true)) shouldBe sample1
        }
        "Real Input" in {
          day.part1(readLines(num, 1)) shouldBe answer1
        }
      }
      "Part 2" - {
        "Sample Input" in {
          day.part2(readLines(num, 2, sample = true)) shouldBe sample2
        }
        "Real Input" in {
          day.part2(readLines(num, 2)) shouldBe answer2
        }
      }
    }
  }
}
