package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.Eq

object Main extends AocAppStreamed(5, DoPart.Two):

  def points(s: Point, f: Point): List[Point] =
    if (s === f) List(s)
    else if (s.x === f.x)
      val np = Point(s.x, if (s.y < f.y) s.y + 1 else s.y - 1)
      s :: points(np, f)
    else if (s.y === f.y)
      val np = Point(if (s.x < f.x) s.x + 1 else s.x - 1, s.y)
      s :: points(np, f)
    else
      val np = Point(
        if (s.x < f.x) s.x + 1 else s.x - 1,
        if (s.y < f.y) s.y + 1 else s.y - 1
      )
      s :: points(np, f)

  case class Point(x: Int, y: Int)
  object Point:
    given Eq[Point] = Eq.by(v => v.x -> v.y)

  case class Vtor(start: Point, finish: Point)

  val parser =
    val num = Rfc5234.digit.rep.map(_.mkString_("").toInt)
    val tuple =
      (num <* Parser.char(',')).flatMap(x => num.tupleLeft(x)).map(Point.apply)
    (tuple <* Parser.string("->").surroundedBy(Rfc5234.sp))
      .flatMap(x => tuple.tupleLeft(x))
      .map(Vtor.apply)

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .flatMap(v =>
        Stream.fromEither[IO](
          parser.parse(v).leftMap(m => new Exception(m.toString))
        )
      )
      .map(_._2)
      .filter(v => v.start.x === v.finish.x || v.start.y === v.finish.y)
      .compile
      .fold(Map[Point, Int]())((acc, n) =>
        points(n.start, n.finish).foldLeft(acc)((acc, n) =>
          acc.updatedWith(n) {
            case None    => Some(1)
            case Some(x) => Some(x + 1)
          }
        )
      )
      .map(_.toList.count(_._2 >= 2).toString)

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .flatMap(v =>
        Stream.fromEither[IO](
          parser.parse(v).leftMap(m => new Exception(m.toString))
        )
      )
      .map(_._2)
      .compile
      .fold(Map[Point, Int]())((acc, n) =>
        points(n.start, n.finish).foldLeft(acc)((acc, n) =>
          acc.updatedWith(n) {
            case None    => Some(1)
            case Some(x) => Some(x + 1)
          }
        )
      )
      .map(_.toList.count(_._2 >= 2).toString)
