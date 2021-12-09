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

  val numberWires =
    Map(
      0 -> List('a', 'b', 'c', 'e', 'f', 'g'),
      1 -> List('c', 'f'),
      2 -> List('a', 'c', 'd', 'e', 'g'),
      3 -> List('a', 'c', 'd', 'f', 'g'),
      4 -> List('b', 'c', 'd', 'f'),
      5 -> List('a', 'b', 'd', 'f', 'g'),
      6 -> List('a', 'b', 'd', 'e', 'f', 'g'),
      7 -> List('a', 'c', 'f'),
      8 -> List('a', 'b', 'c', 'd', 'e', 'f', 'g'),
      9 -> List('a', 'b', 'c', 'd', 'f', 'g')
    )

  val wireNumbers = numberWires.toList
    .map(_.swap)
    .toMap

  def dedooce(
      inputs: NonEmptyList[NonEmptyList[Char]],
      availableNumbers: List[Int],
      currentMappings: Map[Char, Char],
      unmapped: List[Char]
  ): List[Map[String, Int]] =

    println(inputs)
    println(availableNumbers)
    println(currentMappings)
    println(unmapped)

    def configurations(
        wires: List[Char],
        boop: List[Char]
    ): List[List[(Char, Char)]] =
      def go(
          back: List[Char],
          forward: List[Char],
          l: Char
      ): List[((Char, Char), List[Char])] = forward match
        case Nil => Nil
        case h :: t =>
          ((l -> h) -> (back ::: t)) :: go(back ::: List(h), t, l)

      wires match
        case Nil => Nil
        case h :: t =>
          val a = go(Nil, boop, h)
          a.flatMap((one, others) =>
            configurations(t, others) match
              case Nil => List(List(one))
              case t   => t.map(one :: _)
          )

    inputs match {
      case NonEmptyList(h, t) =>
        val l = h.size match
          case 2 => List(1)
          case 3 => List(7)
          case 4 => List(4)
          case 5 => List(2, 3, 5)
          case 6 => List(0, 6, 9)
          case 7 => List(8)

        val ll = l
          .filter(v => availableNumbers.contains(v))
          .flatMap { (num) =>

            val v = numberWires(num)

            val (unfound, found) = v.partitionEither(v =>
              currentMappings.get(v).tupleLeft(v).toRight(v)
            )

            val newMappings =
              if (unfound.isEmpty)
                List(currentMappings)
              else
                configurations(unfound, unmapped).map(
                  _.toMap ++ currentMappings
                )

            NonEmptyList.fromList(t) match
              case None =>
                newMappings.map(newMapping =>
                  wireNumbers.toList
                    .map((wires, number) =>
                      wires.map(newMapping.apply).mkString -> number
                    )
                    .toMap
                )

              case Some(x) =>
                newMappings.flatMap(newMapping =>
                  dedooce(
                    x,
                    availableNumbers.filter(_ != num),
                    newMapping,
                    unmapped.filterNot(newMapping.contains(_))
                  )
                )

          }

        ll

    }

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .evalMap(s =>
        IO.fromEither(parser.parse(s).leftMap(e => new Exception(s"hek: $e")))
      )
      .map { v =>
        val m = dedooce(
          v._2._1,
          (0 to 9).toList,
          Map.empty,
          List('a', 'b', 'c', 'd', 'e', 'f', 'g')
        )
        val f = m.head
        v._2._2.map(v => f(v.mkString_("")))
      }
      .debug()
      .compile
      .drain
