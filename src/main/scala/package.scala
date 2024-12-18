package org.merlin.aoc2024

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import scalaz.Monoid
import scalaz.std.vector.*
import scalaz.syntax.foldable.*
import scalaz.syntax.functor.*

import scala.collection.AbstractIterator
import scala.compiletime.uninitialized

// number extensions

extension (self: Int)
  inline def %%(n: Int): Int =
    val mod = self % n
    if mod < 0 then mod + n else mod
  inline def even: Boolean   = self % 2 == 0

extension (self: Long)
  inline def >=<(n: Long): Boolean       = self >= 0 && self < n
  inline def >=<(n: (Int, Int)): Boolean = self >= n._1 && self < n._2
  inline def %%(n: Long): Long           =
    val mod = self % n
    if mod < 0 then mod + n else mod
  inline def |-|(n: Long): Long          = (self - n).abs
  inline def /%(n: Long): (Long, Long)   = (self / n, self % n)
  inline def **(n: Long): Long           = math.pow(self.doubleValue, n.doubleValue).longValue
  inline def digits: Int                 = 1 + math.log10(self.doubleValue).intValue

// iterator extensions

extension [A](self: Iterator[A])
  def nth(n: Int): A                         = self.drop(n).next
  def findMap[B](f: A => Option[B]): B       = self.flatMap(f).next()
  def findPF[B](f: PartialFunction[A, B]): B = findMap(f.lift)
  def foldMap[B: Numeric](f: A => B): B      = self.map(f).sum

  def takeUntil(p: A => Boolean): Iterator[A] = new AbstractIterator[A]:
    private var hd: A              = uninitialized
    private var hdDefined: Boolean = false
    private var tail: Iterator[A]  = self

    def hasNext: Boolean = hdDefined || tail.hasNext && {
      hd = tail.next()
      hdDefined = true
      if p(hd) then tail = Iterator.empty
      true
    }

    def next(): A = if hasNext then { hdDefined = false; hd }
    else Iterator.empty.next()

private val WordRe = "\\S+".r

// string extensions

extension (self: String)
  def numbers: Vector[Long]          = NumRe.findAllIn(self).map(_.toLong).toVector
  def words: Vector[String]          = WordRe.findAllIn(self).toVector
  def commaSeparated: Vector[String] = self.split(',').toVector
  def characters: Vector[String]     = self.split("").toVector

// vector extensions

extension (self: Vector[String]) def numbers: Vector[Vector[Long]] = self.map(_.numbers)

extension [A](self: Vector[A])
  def mapToMap[B, C](f: A => (B, C)): Map[B, C] = self.map(f).toMap

  def collectToMap[B, C](pf: PartialFunction[A, (B, C)]): Map[B, C] = self.collect(pf).toMap

  def collectToSet[B](pf: PartialFunction[A, B]): Set[B] = self.collect(pf).toSet

  def mapTo[B](f: A => B): Map[A, B] = self.fproduct(f).toMap

  def flatFoldMap[B](f: A => Iterable[B])(using Monoid[B]): B = self.flatMap(f).suml

  def findMap[B](f: A => Option[B]): B = self.flatMap(f).head

  // maps a vector with an accumulator, returning the final accumulator and values
  def mapAcc[B, C](c0: C)(f: (C, A) => (C, B)): (C, Vector[B]) =
    self.foldLeft(c0 -> Vector.empty[B]):
      case ((c, bs), a) => f(c, a) match { case (c2, b) => (c2, bs :+ b) }

  // stateful map, maps a vector with an accumulator then drops the accumulator at the end
  def mapS[B, C](c0: C)(f: (C, A) => (C, B)): Vector[B] = mapAcc(c0)(f)._2

  def groupToMap[B, C](using ABC: A <:< (B, C)): Map[B, Vector[C]] = self
    .map(ABC)
    .foldLeft(Map.empty[B, Vector[C]]):
      case (bc, (b, c)) =>
        bc.updatedWith(b):
          case None     => Some(Vector(c))
          case Some(cs) => Some(cs :+ c)

  // all non-self element pairs
  def allPairs: Vector[(A, A)] = self.tails.toVector.tail.flatMap(self.zip)

  def splice(from: Int, length: Int, insert: Vector[A] = Vector.empty): Vector[A] =
    self.slice(0, from) ++ insert ++ self.slice(from + length, self.length)

  def middle: A = self(self.length / 2)

  def get(i: Int): Option[A] = Option.when(i >= 0 && i < self.length)(self(i))

// range extensions
extension (self: NumericRange[Long])
  def splitLess(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((limit - self.head).toInt)

  def splitGreater(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((1 + limit - self.head).toInt).swap

  def range: Long = if (self.isEmpty) 0 else 1 + self.last - self.head

// a board is a vector of strings

type Board = Vector[String]

extension (self: Board)
  def width                            = self.head.length
  def height                           = self.length
  def nw: Loc                          = Origin
  def se: Loc                          = Loc(width - 1, height - 1)
  def apply(loc: Loc): Char            = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char]      = Option.when(loc >=< self)(self(loc))
  def is(loc: Loc, c: Int): Boolean    = loc >=< self && self(loc) == c
  def is(a: Loc, b: Loc): Boolean      = get(a) == get(b)
  def find(char: Char): Loc            = locations.find(apply(_) == char).get
  def findAll(char: Char): Vector[Loc] = locations.filter(apply(_) == char)

  def locations: Vector[Loc] =
    self.indices.toVector.flatMap(y => self.head.indices.map(x => Loc(x, y)))

  def update(loc: Loc, c: Char): Vector[String] =
    self.updated(loc.y.toInt, self(loc.y.toInt).updated(loc.x.toInt, c))

  def split: (Board, Board) =
    val index = self.indexWhere(_.isEmpty)
    self.slice(0, index) -> self.slice(1 + index, self.length)

enum Dir(val dx: Long, val dy: Long):
  def cw: Dir             = Dir.fromOrdinal((ordinal + 2) % Dir.values.length)
  def ccw: Dir            = Dir.fromOrdinal((ordinal + Dir.values.length - 2) % Dir.values.length)
  def ccw2: Dir           = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % Dir.values.length)
  def reverse: Dir        = Dir.fromOrdinal((ordinal + 4) % Dir.values.length)
  def horizontal: Boolean = dy == 0
  def vertical: Boolean   = dx == 0

  inline def *(length: Long): Vec = Vec(this, length)

  case N  extends Dir(0, -1)
  case NE extends Dir(1, -1)
  case E  extends Dir(1, 0)
  case SE extends Dir(1, 1)
  case S  extends Dir(0, 1)
  case SW extends Dir(-1, 1)
  case W  extends Dir(-1, 0)
  case NW extends Dir(-1, -1)

object Dir:
  val byName: Map[String, Dir] = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N).withDefault(valueOf)

  implicit def toVec(dir: Dir): Vec = dir * 1

  given Ordering[Dir] = Ordering.by(_.ordinal)

val CardinalDirections: Vector[Dir] = Vector(Dir.N, Dir.E, Dir.S, Dir.W)

val OrdinalDirections: Vector[Dir] = Vector(Dir.NE, Dir.SE, Dir.SW, Dir.NW)

// a location in space

final case class Loc(x: Long, y: Long):
  inline def +(addend: Vec): Loc = Loc(x + addend.dx, y + addend.dy)

  inline def -(subtrahend: Vec): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)

  inline def -(subtrahend: Loc): Loc = Loc(x - subtrahend.x, y - subtrahend.y)

  inline def +(addend: Loc): Loc = Loc(x + addend.x, y + addend.y)

  inline def >=<(board: Board): Boolean = this >=< (board.head.length, board.length)

  inline def <>=(board: Board): Boolean = !(this >=< board)

  inline def >=<(size: Long): Boolean = this >=< (size, size)

  inline def >=<(w: Long, h: Long): Boolean = x >=< w && y >=< h

  def adjacents: Vector[Loc] = CardinalDirections.map(this + _)

  def manhattan(l: Loc): Long = (l.x - x).abs + (l.y - y).abs

  override def toString: String = s"$x,$y"

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)

val Origin: Loc = Loc(0, 0)

// a direction and magnitude

final case class Vec(direction: Dir, magnitude: Long):
  def dx: Long = direction.dx * magnitude
  def dy: Long = direction.dy * magnitude

  inline def +(addend: Long): Vec     = copy(magnitude = magnitude + addend)
  inline def -(subtrahend: Long): Vec = copy(magnitude = magnitude - subtrahend)
  inline def *(multiplier: Long): Vec = copy(magnitude = magnitude * multiplier)

// geometries

extension (self: Vector[Vec])
  def vertices: Vector[Loc] = self.scanLeft(Origin)(_ + _)
  def perimeter: Long       = self.map(_.magnitude).sum

extension (self: Vector[Loc]) def area: Long = self.zip(self.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2

object L:
  def unapply(s: String): Option[Long] = s.toLongOption

extension (self: Boolean)
  def flatOption[A](fa: => Option[A]): Option[A] = if self then fa else None