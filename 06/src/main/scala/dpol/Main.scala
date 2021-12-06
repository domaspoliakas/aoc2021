package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data._
import scala.annotation.tailrec

object Main extends AocAppStreamed(6, DoPart.One):

  val parser =
    Rfc5234.digit.rep.map(_.mkString_("").toLong).repSep(Parser.char(','))

  @tailrec
  def go(nums: Map[Long, Long], n: Long): Map[Long, Long] =
    if (n === 0L) nums
    else 
      nums.get(0L) match {
        case Some(zeros) =>
          go(
          nums
            .removed(0L)
            .map(a => (a._1 - 1) -> a._2)
            .updatedWith(6L) { _.map(_ + zeros).orElse(Some(zeros)) }
            .+(8L -> zeros),
            n - 1
          )
            
        case None => 
          go(nums.map(a => (a._1 - 1) -> a._2), n - 1)      
      }

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile.compile.string
      .flatMap { s =>
        IO.fromEither(parser.parse(s).leftMap(_ => new Exception("hek")))
          .map(_._2)
      }
      .map { nums =>
        go(nums.groupBy(identity).mapValues(_.size.toLong).toMap, 80).toList.foldMap(_._2)
      }
      .map(_.toString)

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile.compile.string
      .flatMap { s =>
        IO.fromEither(parser.parse(s).leftMap(_ => new Exception("hek")))
          .map(_._2)
      }
      .map { nums =>
        go(nums.groupBy(identity).mapValues(_.size.toLong).toMap, 256).toList.foldMap(_._2)
      }
      .map(_.toString)
