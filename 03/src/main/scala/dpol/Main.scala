package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import javax.xml.bind.DatatypeConverter
import scala.annotation.tailrec

object Main extends AocAppStreamed(3, DoPart.Two):
  override def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .through(text.lines)
      .map(v =>
        v.toList.collect {
          case '1' => (0, 1)
          case '0' => (1, 0)
        }
      )
      .filter(_.nonEmpty)
      .compile
      .fold(List.fill(12)(0 -> 0))((_, _).parMapN(_ |+| _))
      .map { v =>
        val (a, b) = v.map { case t @ (a, b) =>
          if (a > b) (0, 1) else (1, 0)
        }.unzip
        Integer
          .parseInt(a.mkString, 2)
          .*(Integer.parseInt(b.mkString, 2))
          .toString
      }

  override def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .through(text.lines)
      .filter(_.nonEmpty)
      .compile
      .toList
      .flatMap(v =>
        (
          IO(Integer.parseInt(v(go(v.zipWithIndex, true)), 2)),
          IO(Integer.parseInt(v(go(v.zipWithIndex, false)), 2))
        ).parMapN(_ * _).map(_.toString)
      )

  @tailrec
  def go(input: List[(String, Int)], c: Boolean): Int =
    val (a, b) = input
      .map(_._1)
      .foldMap(_.charAt(0) match {
        case '0' => (1, 0)
        case '1' => (0, 1)
        case _   => sys.error("hek")
      })

    val filter =
      if (c)
        if (a > b)
          "0"
        else
          "1"
      else if (a > b)
        "1"
      else
        "0"

    input.filter(_._1.startsWith(filter)) match {
      case List((_, i)) => i
      case l            => go(l.map(_.leftMap(_.tail)), c)
    }
