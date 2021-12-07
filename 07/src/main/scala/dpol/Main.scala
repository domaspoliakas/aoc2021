package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._

object Main extends AocAppStreamed(7, DoPart.Two):

  val csvparse =
    Rfc5234.digit.rep.map(_.mkString_("").toInt).repSep(Parser.char(','))

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .flatMap(v =>
        Stream.fromEither[IO](
          csvparse.parse(v).leftMap(_ => new Exception("hek"))
        )
      )
      .map(_._2)
      .map(l =>
        val max = l.maximum
        val min = l.minimum

        val aa = (for (a <- min to max) yield {
          a -> (l.map(v => Math.abs(v - a)).sumAll)
        }).minBy(_._2)

        aa
      )
      .compile
      .last

  def part2(inputFile: Stream[IO, String]): IO[String] =

    def cost(i: Int): Int = 
      ((Math.pow(i, 2) + i) / 2).toInt

    inputFile
      .filter(_.nonEmpty)
      .flatMap(v =>
        Stream.fromEither[IO](
          csvparse.parse(v).leftMap(_ => new Exception("hek"))
        )
      )
      .map(_._2)
      .map(l =>
        val max = l.maximum
        val min = l.minimum

        val aa = (for (a <- min to max) yield {
          a -> (l.map(v => cost(Math.abs(v - a))).sumAll)
        }).minBy(_._2)

        aa
      )
      .compile
      .last
