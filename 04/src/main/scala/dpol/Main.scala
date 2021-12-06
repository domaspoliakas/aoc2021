package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import scala.annotation.tailrec
import cats.parse._

object Main extends AocAppStreamed(4, DoPart.Two):

  def check(card: List[Option[Int]]): Boolean =
    (for { a <- 0 to 4 } yield {
      val aa = a * 5
      val row = card(aa + 0).isEmpty && card(aa + 1).isEmpty && card(
        aa + 2
      ).isEmpty && card(aa + 3).isEmpty && card(aa + 4).isEmpty

      val col = card(0 + a).isEmpty && card(5 + a).isEmpty && card(
        10 + a
      ).isEmpty && card(15 + a).isEmpty && card(20 + a).isEmpty

      col || row
    }).contains(true)

  def mark(card: List[Option[Int]], num: Int): List[Option[Int]] = card match {
    case Some(x) :: t if x == num => None :: t
    case h :: t                   => h :: mark(t, num)
    case Nil                      => Nil
  }

  def go(cards: List[List[Option[Int]]], nums: List[Int]): Int = {
    nums match {
      case h :: t =>
        val potato = cards.map(card =>
          val marked = mark(card, h)
          marked -> check(marked)
        )

        potato
          .find(_._2)
          .map(_._1.flatten.sum * h)
          .getOrElse(
            go(potato.map(_._1), t)
          )

      case _ =>
        sys.error("hek3")
    }
  }

  val parseroo =
    Rfc5234.digit.rep
      .surroundedBy(Rfc5234.sp.rep0)
      .map(_.mkString_("").toInt)
      .rep

  override def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile.pull.uncons1
      .flatMap {
        case Some(h, t) =>
          h.split(",").toList.traverse(_.toIntOption) match {
            case None =>
              Pull.raiseError[IO](new Exception("hek"))
            case Some(nums) =>
              Pull
                .eval(
                  t
                    .filter(_.nonEmpty)
                    .chunkN(5, false)
                    .map { chunk =>
                      chunk.toList
                        .traverse(s => parseroo.parse(s))
                        .leftMap(err => new Exception(err.toString))
                    }
                    .rethrow
                    .map(_.flatMap(_._2.toList))
                    .compile
                    .toList
                    .map(cards =>
                      go(cards.filter(_.nonEmpty).map(_.map(_.some)), nums)
                    )
                )
                .flatMap(Pull.output1(_))
          }

        case None => Pull.raiseError[IO](new Exception("hek"))
      }
      .void
      .stream
      .compile
      .last
      .map(_.toString)

  def go2(
      cards: List[List[Option[Int]]],
      nums: List[Int]
  ): Option[Int] = {
    nums match {
      case Nil => None
      case h :: t =>
        val potato = cards.map(card =>
          val marked = mark(card, h)
          marked -> check(marked)
        )

        val (y, n) = potato.partition(_._2)

        val g = go2(n.map(_._1), t)

        if (g.isEmpty)
          y.headOption.map(_._1.flatten.sum * h)
        else
          g
    }
  }

  override def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile.pull.uncons1
      .flatMap {
        case Some(h, t) =>
          h.split(",").toList.traverse(_.toIntOption) match {
            case None =>
              Pull.raiseError[IO](new Exception("hek"))
            case Some(nums) =>
              Pull
                .eval(
                  t
                    .filter(_.nonEmpty)
                    .chunkN(5, false)
                    .map { chunk =>
                      chunk.toList
                        .traverse(s => parseroo.parse(s))
                        .leftMap(err => new Exception(err.toString))
                    }
                    .rethrow
                    .map(_.flatMap(_._2.toList))
                    .compile
                    .toList
                    .map(cards =>
                      go2(cards.filter(_.nonEmpty).map(_.map(_.some)), nums)
                    )
                )
                .flatMap(Pull.output1(_))
          }

        case None => Pull.raiseError[IO](new Exception("hek"))
      }
      .void
      .stream
      .compile
      .last
      .map(_.toString)
