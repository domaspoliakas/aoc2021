package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._

object Main extends AocApp(3, DoPart.One):

  def part1(inputFile: Path): IO[String] =
    IO.unit

  def part2(inputFile: Path): IO[String] =
    IO.unit
