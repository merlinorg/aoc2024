package org.merlin.aoc2024

object Day9 extends AoC:

  override def part1(lines: Vector[String]): Long =
    val (files, blanks) = parse(lines)
    Iterator
      .unfold((files.reverse, blanks)):
        case (file +: files, blank +: blanks) if blank.pos < file.pos =>
          if (blank.len == file.len)
            Some(file.inBlank(blank), (files, blanks))
          else if (blank.len < file.len)
            Some(file.inBlank(blank), (file.dropRight(blank.len) +: files, blanks))
          else
            Some(file.inBlank(blank), (files, blank.dropLeft(file.len) +: blanks))
        case (file +: files, blanks)                                  =>
          Some(file, (files, blanks))
        case _                                                        => None
      .map(_.value)
      .sum
  end part1

  override def part2(lines: Vector[String]): Long =
    val (files, blanks) = parse(lines)
    Iterator
      .unfold((files.reverse, blanks)):
        case (file +: files, blanks) =>
          blanks.zipWithIndex.find:
            case (blank, _) => blank.pos < file.pos && blank.len >= file.len
          match
            case Some(blank, index) =>
              val insert = if (file.len == blank.len) Vector.empty else Vector(blank.dropLeft(file.len))
              Some(file.inBlank(blank), (files, blanks.splice(index, 1, insert)))
            case None               =>
              Some(file, (files, blanks))
        case _                       => None
      .map(_.value)
      .sum
  end part2

  private def parse(lines: Vector[String]): (Vector[Extent], Vector[Extent]) =
    lines.head.toVector
      .foldLeft((true, 0, 0, (Vector.empty[Extent], Vector.empty[Extent]))):
        case ((true, index, pos, (files, blanks)), char)  =>
          (false, index, pos + char.asDigit, (files :+ Extent(index, pos, char.asDigit), blanks))
        case ((false, index, pos, (files, blanks)), char) =>
          (true, index + 1, pos + char.asDigit, (files, blanks :+ Extent(index, pos, char.asDigit)))
      ._4

  private final case class Extent(id: Long, pos: Int, len: Int):
    def value: Long                     = id * (pos * len + len * (len - 1) / 2)
    def inBlank(extent: Extent): Extent = copy(pos = extent.pos, len = len min extent.len)
    def dropLeft(n: Int): Extent        = copy(pos = pos + n, len = len - n)
    def dropRight(n: Int): Extent       = copy(len = len - n)

end Day9
