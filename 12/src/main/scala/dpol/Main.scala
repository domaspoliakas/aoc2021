package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data.NonEmptyList
import cats.kernel.Eq

enum Node:
  case Start
  case End
  case BigCave(label: String)
  case SmallCave(label: String)

object Node:
  given Eq[Node] = Eq.fromUniversalEquals

object Main extends AocAppStreamed(12, DoPart.Two):

  val node: Parser[Node] =
    Parser.string("start").as(Node.Start) orElse
      Parser.string("end").as(Node.End) orElse
      Parser
        .charIn('A' to 'Z')
        .rep
        .map(v => Node.BigCave(v.mkString_(""))) orElse
      Parser.charIn('a' to 'z').rep.map(v => Node.SmallCave(v.mkString_("")))

  val path: Parser[(Node, Node)] = (node <* Parser.char('-'), node).tupled

  def paths(
      curr: NonEmptyList[Node],
      all: Map[Node, Set[Node]],
      checkeroo: (Node.SmallCave, NonEmptyList[Node]) => Boolean
  ): List[List[Node]] =
    all
      .get(curr.head)
      .fold(List(curr.toList))(nodes =>
        nodes.toList.collect {
          case n: Node.BigCave =>
            paths(NonEmptyList(n, curr.toList), all, checkeroo)

          case n: Node.SmallCave if checkeroo(n, curr) =>
            paths(NonEmptyList(n, curr.toList), all, checkeroo)

          case Node.End =>
            List(NonEmptyList(Node.End, curr.toList).toList)

        }.flatten
      )

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .through(parsed(path))
      .compile
      .toList
      .map(l =>
        val graph = l.foldLeft(Map.empty[Node, Set[Node]])((acc, next) =>
          acc
            .updatedWith(next._1)(_.map(_ + next._2).orElse(Some(Set(next._2))))
            .updatedWith(next._2)(_.map(_ + next._1).orElse(Some(Set(next._1))))
        )

        paths(
          NonEmptyList.one(Node.Start),
          graph,
          (a, b) => !b.contains_(a)
        ).size.toString
      )

  def checkp2(n: Node.SmallCave, curr: List[Node]): Boolean =
    val g = curr.collect {
      case Node.SmallCave(a) => a
    }.groupBy(identity)

    !g.exists(_._2.size == 2) || !g.contains(n.label)

  def part2(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .through(parsed(path))
      .compile
      .toList
      .map(l =>
        val graph = l.foldLeft(Map.empty[Node, Set[Node]])((acc, next) =>
          acc
            .updatedWith(next._1)(_.map(_ + next._2).orElse(Some(Set(next._2))))
            .updatedWith(next._2)(_.map(_ + next._1).orElse(Some(Set(next._1))))
        )

        paths(
          NonEmptyList.one(Node.Start),
          graph,
          (a, b) => checkp2(a, b.toList)
        ).size.toString
      )
