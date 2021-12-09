package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.kernel.Order

opaque type Wire = String
object Wire:
  def apply(c: String): Wire = c
  def asString(w: Wire): String = w
  extension (w: Wire)
    def char: Char = w.charAt(0)

opaque type Display = String
object Display:
  def apply(c: String): Display = c
  def asString(w: Display): String = w
  extension (w: Display)
    def char: Char = w.charAt(0)

object Main extends AocAppStreamed(8, DoPart.Two):

  val parser =
    (
      (Rfc5234.alpha.rep <* Rfc5234.sp).repUntil(Parser.char('|')) <* Parser
        .char('|') <* Rfc5234.sp,
      (Rfc5234.alpha.rep <* Rfc5234.sp.rep0).rep(4, 4)
    ).tupled

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .evalMap(s =>
        IO.fromEither(parser.parse(s).leftMap(e => new Exception(s"hek: $e")))
      )
      .map(_._2._2)
      .map(
        _.toList.filter { v =>
          val size = v.size
          size == 2 || size == 4 || size == 3 || size == 7
        }
      )
      .map(_.size)
      .compile
      .foldMonoid

  val all = List("a", "b", "c", "d", "e", "f", "g")

  val numberWires =
    Map(
      0 -> List("a", "b", "c", "e", "f", "g"),
      1 -> List("c", "f"),
      2 -> List("a", "c", "d", "e", "g"),
      3 -> List("a", "c", "d", "f", "g"),
      4 -> List("b", "c", "d", "f"),
      5 -> List("a", "b", "d", "f", "g"),
      6 -> List("a", "b", "d", "e", "f", "g"),
      7 -> List("a", "c", "f"),
      8 -> all,
      9 -> List("a", "b", "c", "d", "f", "g")
    ).view.mapValues(_.map(Wire(_))).toMap

  val numberWireM: Map[Set[Char], Int] =
    numberWires.toList.map((num, s) =>
        s.map(_.char).toSet -> num
      ).toMap

  val wireNumbers = numberWires.toList
    .map(_.swap)
    .toMap

  def deduc(
      mapped: Map[Wire, Set[Display]],
      a: List[(List[List[Wire]], List[Display])]
  ): List[Map[Wire, Set[Display]]] = a match
    case (possibleWiresMany, observedDisplays) :: t =>
      possibleWiresMany.flatMap { possibleWires =>
        val r = mapped.map { case (wire, possibleDisplays) =>
          wire -> (if (possibleWires.contains(wire))
                     possibleDisplays.intersect(observedDisplays.toSet)
                   else
                     possibleDisplays.removedAll(observedDisplays))
        }

        if (r.exists(_._2.isEmpty))
          List()
        else
          deduc(r, t)
      }
    case Nil =>
      List(mapped)


  def part2(inputFile: Stream[IO, String]): IO[String] =
    val olinho = all.map(v => Wire(v) -> all.toSet.map(v => Display(v))).toMap

    inputFile
      .filter(_.nonEmpty)
      .evalMap(s =>
        IO.fromEither(parser.parse(s).leftMap(e => new Exception(s"hek: $e")))
      )
      .evalMap { v =>
        val potato = v._2._1.map { h =>
          h.map(v => Display(v.toString)) -> (h.size match
            case 2 => List(1)
            case 3 => List(7)
            case 4 => List(4)
            case 5 => List(2, 3, 5)
            case 6 => List(0, 6, 9)
            case 7 => List(8)
          )
        }.map { (display, nums) =>
          nums.map(numberWires(_)) -> display.toList
        }.toList

        IO.fromOption(deduc(
          olinho,
          potato
        ).find(_.forall(_._2.size == 1)).map(s => s.view.mapValues(_.head).toMap))(new Exception("no good")).tupleRight(v._2._2)
      }.map((mappingFunc, inputs) =>
        val remap = mappingFunc.toList.map(_.swap.bimap(_.char, _.char)).toMap
        inputs.map { v => 
          numberWireM(v.map(remap).toList.toSet)
        }
      ).map(_.mkString_("").toInt)
      .compile
      .foldMonoid
