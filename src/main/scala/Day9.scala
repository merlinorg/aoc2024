package org.merlin.aoc2024

object Day9 extends AoC:

  override def part1(lines: Vector[String]): Long =
    val (_, (blanks, files)) = parse(lines)
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
    val (_, (blanks, files)) = parse(lines)
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

  private def parse(lines: Vector[String]): (Int, (Vector[Blank], Vector[File])) =
    lines.head.toVector.zipWithIndex
      .foldLeft((0, (Vector.empty[Blank], Vector.empty[File]))):
        case ((pos, (blanks, files)), (char, index)) if index % 2 == 1 =>
          (pos + char.asDigit, (blanks :+ Blank(pos, char.asDigit), files))
        case ((pos, (blanks, files)), (char, index)) =>
          (pos + char.asDigit, (blanks, files :+ File(index / 2, pos, char.asDigit)))

  sealed trait Extent

  private case class File(id: Long, pos: Int, len: Int) extends Extent:
    def value: Long                 = id * (pos * len + len * (len - 1) / 2)
    def dropRight(n: Int): File     = copy(len = len - n)
    def inBlank(blank: Blank): File = copy(pos = blank.pos, len = len min blank.len)

  private case class Blank(pos: Int, len: Int) extends Extent:
    def dropLeft(n: Int): Blank = Blank(pos + n, len - n)

end Day9
