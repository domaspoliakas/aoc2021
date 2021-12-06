package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Main extends AocApp(1):
  def part1(inputFile: Path): IO[String] =
    asNumbers(inputFile).zipWithPrevious
      .collect {
        case (Some(x), y) if y > x => y
      }
      .compile
      .count

  def part2(inputFile: Path): IO[String] =
    asNumbers(inputFile)
      .sliding(3, 1)
      .map(_.sumAll)
      .zipWithPrevious
      .collect {
        case (Some(x), y) if y > x => y
      }
      .compile
      .count
