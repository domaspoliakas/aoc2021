package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data.State

object Main extends AocAppStreamed(11, DoPart.Two):

  val mods =
    List(-1 -> -1, 0 -> -1, 1 -> -1, -1 -> 0, 1 -> 0, -1 -> 1, 0 -> 1, 1 -> 1)

  def step(grid: Map[(Int, Int), Int]): State[Int, Map[(Int, Int), Int]] =
    def go(g: Map[(Int, Int), Int]): State[Int, Map[(Int, Int), Int]] =
      val (flashing, unflashed) = g.partition(_._2 > 9)
      State
        .modify[Int](_ + flashing.size)
        .flatMap(_ =>
          if (flashing.nonEmpty)
            val n = flashing.toList
              .flatMap((point, _) =>
                mods.map((x, y) => (point._1 + x) -> (point._2 + y))
              )
              .foldLeft(unflashed)((acc, n) => acc.updatedWith(n)(_.map(_ + 1)))
            go(n).map(_ ++ flashing.view.mapValues(_ => 0))
          else State.pure(unflashed)
        )

    val g = grid.view.mapValues(_ + 1)
    go(g.toMap)

  def printGrid(g: Map[(Int, Int), Int]): String = g
    .groupBy(_._1._2)
    .toList
    .sortBy(_._1)
    .map((_, b) => b.toList.sortBy(_._1._1).map(_._2))
    .map(_.mkString)
    .mkString("*****\n", "\n", "\n")

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .map(_.toCharArray.toList.map(_.toString.toInt))
      .compile
      .toList
      .map(gridL =>
        val grid = gridL
          .map(_.zipWithIndex)
          .zipWithIndex
          .flatMap((l, y) => l.map((v, x) => (x, y) -> v))
          .toMap

          val n = (1 to 100).foldLeft(grid -> 0)((acc, _) => step(acc._1).run(acc._2).value.swap)

          n._2.toString

      )

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .map(_.toCharArray.toList.map(_.toString.toInt))
      .compile
      .toList
      .map(gridL =>
        val grid = gridL
          .map(_.zipWithIndex)
          .zipWithIndex
          .flatMap((l, y) => l.map((v, x) => (x, y) -> v))
          .toMap

        def go(g: Map[(Int, Int), Int], n: Int): Int =
          val m = step(g).run(0).value._2
          if (m.forall(_._2 == 0))
            n
          else
            go(m, n + 1)

        go(grid, 1).toString

      )
