package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._

object Main extends AocApp(2, DoPart.Two):

  def instruction(in: String): Int => ((Int, Int)) => (Int, Int) = in match {
    case "forward" => incr => t => t.copy(_1 = t._1 + incr)
    case "up" => incr => t => t.copy(_2 = t._2 - incr)
    case "down" => incr => t => t.copy(_2 = t._2 + incr)
    case _ => _ => identity
  }

  def part1(inputFile: Path): IO[String] =
    Files[IO].readAll(inputFile)
      .through(text.utf8Decode)
      .through(text.lines)
      .map(_.split(" "))
      .collect {
        case Array(a, b) if b.toIntOption.isDefined => a -> b.toInt
      }
      .compile
      .fold(0 -> 0) {
        case (coords, (nextInstruction, num)) =>
          instruction(nextInstruction)(num)(coords)
      }.map(v => (v._1 * v._2).show)


  def instruction2(in: String): Int => ((Int, Int, Int)) => (Int, Int, Int) = in match {
    case "forward" => incr => t => t.copy(_1 = t._1 + incr, _2 = t._2 + (t._3 * incr))
    case "up" => incr => t => t.copy(_3 = t._3 - incr)
    case "down" => incr => t => t.copy(_3 = t._3 + incr)
    case _ => _ => identity
  }

  def part2(inputFile: Path): IO[String] =
    Files[IO].readAll(inputFile)
      .through(text.utf8Decode)
      .through(text.lines)
      .map(_.split(" "))
      .collect {
        case Array(a, b) if b.toIntOption.isDefined => a -> b.toInt
      }
      .compile
      .fold((0, 0, 0)) {
        case (coords, (nextInstruction, num)) =>
          instruction2(nextInstruction)(num)(coords)
      }.map(v => (v._1 * v._2).show)
