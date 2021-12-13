package dpol

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse.Parser

enum DoPart:
  case One
  case Two
  case Both

trait AocApp(day: Int, part: DoPart = DoPart.Both) extends IOApp.Simple:

  private val dayF = if (day < 10) s"0$day" else s"$day"

  given [A](using Show[A]): Conversion[IO[A], IO[String]] with
    def apply(ioA: IO[A]): IO[String] = ioA.map(_.show)

  def diffInput: Boolean = false

  override def run: IO[Unit] =
    IO.whenA(part == DoPart.One || part == DoPart.Both)(
      IO.println("Part 1:") >>
        part1(Path(show"/home/dpol/misc/aoc2021/$dayF/input${if (!diffInput) ""
        else "1"}.txt")).flatMap(v => IO.println(show"$v"))
    ) >>
      IO.whenA(part == DoPart.Two || part == DoPart.Both)(
        IO.println("Part 2:") >>
          part2(
            Path(show"/home/dpol/misc/aoc2021/$dayF/input${if (!diffInput) ""
            else "2"}.txt")
          ).flatMap(v => IO.println(show"$v"))
      )

  def part1(inputFile: Path): IO[String]
  def part2(inputFile: Path): IO[String]

  def asNumbers(inputFile: Path): Stream[IO, Int] =
    val files = fs2.io.file.Files[IO]
    files
      .readAll(inputFile)
      .through(text.utf8Decode)
      .through(text.lines)
      .map(_.toIntOption)
      .unNone

abstract class AocAppStreamed(day: Int, part: DoPart = DoPart.Both)
    extends IOApp.Simple:

  private val dayF = if (day < 10) s"0$day" else s"$day"

  given [A](using Show[A]): Conversion[IO[A], IO[String]] with
    def apply(ioA: IO[A]): IO[String] = ioA.map(_.show)

  def diffInput: Boolean = false

  override def run: IO[Unit] =
    IO.whenA(part == DoPart.One || part == DoPart.Both)(
      IO.println("Part 1:") >>
        part1(
          toStream(
            Path(show"/home/dpol/misc/aoc2021/$dayF/input${if (!diffInput) ""
            else "1"}.txt")
          )
        ).flatMap(v => IO.println(show"$v"))
    ) >>
      IO.whenA(part == DoPart.Two || part == DoPart.Both)(
        IO.println("Part 2:") >>
          part2(
            toStream(
              Path(show"/home/dpol/misc/aoc2021/$dayF/input${if (!diffInput) ""
              else "2"}.txt")
            )
          ).flatMap(v => IO.println(show"$v"))
      )

  private def toStream(inputFile: Path): Stream[IO, String] =
    Files[IO].readAll(inputFile).through(text.utf8Decode).through(text.lines)

  def parsed[A](parser: Parser[A]): Pipe[IO, String, A] = _.evalMap(v =>
    IO.fromEither(
      parser.parse(v).leftMap(err => new Exception(err.toString)).map(_._2)
    )
  )

  def part1(inputFile: Stream[IO, String]): IO[String]
  def part2(inputFile: Stream[IO, String]): IO[String]
