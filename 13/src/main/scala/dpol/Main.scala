package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._

enum FoldInstruction:
  case X(i: Int)
  case Y(i: Int)

object Main extends AocAppStreamed(13, DoPart.Two):

  val num = Rfc5234.digit.rep.mapFilter(_.mkString_("").toIntOption)
  val pointParser: Parser[(Int, Int)] = (num <* Parser.char(','), num).tupled
  val foldParser: Parser[FoldInstruction] =
    val fold = Parser.string("fold along ")
    val y = Parser.string("y=") *> num.map(FoldInstruction.Y(_))
    val x = Parser.string("x=") *> num.map(FoldInstruction.X(_))
    fold *> (y orElse x)

  def print(l: List[(Int, Int)]): String =
    val maxX = l.maxBy(_._1)._1
    val maxY = l.maxBy(_._2)._2
    (for {
      y <- 0 to maxY
      x <- 0 to maxX
    } yield 
      val s = s"${if (x == 0) "\n" else ""}${if l.contains((x, y)) then "#" else "."}"
      // println(s"${(x, y).toString}: $s")
      s
    ).mkString

  def go(inputFile: Stream[IO, String]): IO[(List[(Int, Int)], List[FoldInstruction])] =
    inputFile.compile
      .fold(
        (List.empty[(Int, Int)], List.empty[FoldInstruction], false)
          .asRight[Throwable]
      )((acc, next) =>
        acc.flatMap((points, instr, reachedInstructions) =>
          if (next.isEmpty)
            Right((points, instr, true))
          else if (reachedInstructions)
            foldParser
              .parse(next)
              .leftMap(err => new Exception(err.toString))
              .map(v => (points, v._2 :: instr, reachedInstructions))
          else
            pointParser
              .parse(next)
              .leftMap(err => new Exception(err.toString))
              .map(v => (v._2 :: points, instr, reachedInstructions))
        )
      )
      .rethrow
      .map((points, instr, _) => (points.reverse, instr.reverse))

  def foldoo(points: List[(Int, Int)], instr: FoldInstruction): List[(Int, Int)] = instr match 
    case FoldInstruction.X(fx) => 
      points.map((x, y) => if (x > fx) (fx * 2 - x, y) else (x, y)).distinct
    case FoldInstruction.Y(fy) => 
      points.map((x, y) => if (y > fy) (x, fy * 2 - y) else (x, y)).distinct

  def part1(inputFile: Stream[IO, String]): IO[String] =
    go(inputFile)
      .map { (points, instr) =>
        instr.headOption match 
          case Some(f) => 
            val res = foldoo(points, f)
            s"${print(res)}\n${res.size}"
          case None => 
            "no instructions detected"
      }

  def part2(inputFile: Stream[IO, String]): IO[String] =
    go(inputFile).map { (points, instr) =>
      val res = instr.foldLeft(points)((acc, next) => foldoo(acc, next)) 
      print(res)
    }
