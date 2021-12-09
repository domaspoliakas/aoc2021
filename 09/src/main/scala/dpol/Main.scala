package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data.NonEmptyList
import scala.annotation.tailrec

enum Basin:
  case HighPoint
  case LowPoint
  case Dir(x: Int, y: Int)

object Main extends AocAppStreamed(9, DoPart.Two):

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .evalMap(a =>
        IO.fromEither(
          Rfc5234.digit.rep
            .map(_.map(_.toString.toInt))
            .parse(a)
            .leftMap(e => new Exception(e.toString))
        ).map(_._2)
      )
      .zipWithPreviousAndNext
      .map { case (previous, current, next) =>
        ((none[Int], none[Int], none[Int]) ::

          (previous
            .fold(List.empty[Int])(_.toList)
            .padZipWith(
              current.toList
                .padZip(next.fold(List.empty[Int])(_.toList))
            ) { (p, b) =>
              val (c, n) = b.unzip
              (p, c.flatten, n.flatten)
            }) ::: List((none[Int], none[Int], none[Int]))).sliding(3).map {
          case a :: (tm, mm, bm) :: b :: Nil =>
            val others = a.toList ::: List(tm, bm) ::: b.toList

            mm.filter(i => others.forall(_.fold(true)(_ > i)))

          case _ => None
        }
      }
      .flatMap(v => Stream.emits(v.toSeq))
      .unNone
      .map(_ + 1)
      .compile
      .foldMonoid

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .evalMap(a =>
        IO.fromEither(
          Rfc5234.digit.rep
            .map(_.map(_.toString.toInt))
            .parse(a)
            .leftMap(e => new Exception(e.toString))
        ).map(_._2)
      )
      .zipWithIndex
      .zipWithPreviousAndNext
      .map { case (previous, (currentRow, currentIndex), next) =>

        val (previousRow, previousIndex) = previous
          .map(_.bimap(_.toList, _.some))
          .getOrElse(List.empty[Int] -> None)
        val (nextRow, nextIndex) =
          next.map(_.bimap(_.toList, _.some)).getOrElse(List.empty[Int] -> None)

        ((
          none[(Int, (Int, Long))],
          none[(Int, (Int, Long))],
          none[(Int, (Int, Long))]
        ) ::
          (previousRow.zipWithIndex
            .map((value, index) => value -> (index, previousIndex.get))
            .padZipWith(
              currentRow.toList.zipWithIndex
                .map((value, index) => value -> (index, currentIndex))
                .padZip(
                  nextRow.zipWithIndex.map((value, index) =>
                    value -> (index, nextIndex.get)
                  )
                )
            ) { (p, b) =>
              val (c, n) = b.unzip
              (p, c.flatten, n.flatten)
            }) ::: List(
            (
              none[(Int, (Int, Long))],
              none[(Int, (Int, Long))],
              none[(Int, (Int, Long))]
            )
          )).sliding(3).map {
          case a :: (tm, mm, bm) :: b :: Nil =>

            mm.map {
              case (9, coord) => 
                coord -> Basin.HighPoint
              case (value, coord) =>
                NonEmptyList.fromList(List(a._2, tm, bm, b._2).collect {
                  case Some(neightbourValue, neighbourCoord) if neightbourValue < value => neightbourValue -> neighbourCoord
                  }).fold(coord -> Basin.LowPoint){v => 
                  val (_, dirCoord) = v.minimumBy(_._1)
                  coord -> Basin.Dir(dirCoord._1, dirCoord._2.toInt)
                }
            }

          case _ => None
        }

      }
      .flatMap(v => Stream.emits(v.toSeq))
      .unNone
      .compile
      .toList
      .map { v =>
        val mapp = v.toMap

        @tailrec
        def follow(x: Int, y: Int): Option[(Int, Int)] =
          mapp.get((x, y.toLong)) match 
            case None => None
            case Some(Basin.HighPoint) => None
            case Some(Basin.LowPoint) => Some(x -> y)
            case Some(Basin.Dir(xx, yy)) => follow(xx, yy)

        val res = v.foldLeft(Map.empty[(Int, Int), Int])((acc, next) =>
            follow(next._1._1, next._1._2.toInt).fold(acc)((a) => acc.updatedWith(a)(_.fold(1)(_ + 1).some))
            )

        val res2 = res.toList.sortBy(_._2).reverse

        (res2(0)._2 * res2(1)._2 * res2(2)._2).toString

      }
