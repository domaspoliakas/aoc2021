package dpol

import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import fs2._
import fs2.io.file._
import cats.parse._
import cats.data.NonEmptyList
import scala.annotation.tailrec

object Main extends AocAppStreamed(10, DoPart.Two):

  enum Brack:
    case Parens(l: List[Brack])
    case Square(l: List[Brack])
    case Curly(l: List[Brack])
    case Angle(l: List[Brack])

  def fullo = brack.rep <* Parser.end

  def brack: Parser[Brack] =
    (brackP('<', '>', Brack.Angle(_)) orElse
      brackP('(', ')', Brack.Parens(_)) orElse
      brackP('[', ']', Brack.Square(_)) orElse
      brackP('{', '}', Brack.Curly(_)))

  def brackP(open: Char, close: Char, f: List[Brack] => Brack) =
    (Parser
      .char(open)
      .flatMap(_ => brack.rep0.withContext(close.toString))
      .flatMap(a => Parser.char(close).withContext(close.toString).as(a)))
      .map(f)

  def part1(inputFile: Stream[IO, String]): IO[String] =
    inputFile
      .filter(_.nonEmpty)
      .map { v =>
        val r = brack.parse(v)
        println(r)
        r match
          case Left(Parser.Error(offset, _)) if offset < v.length =>
            v.charAt(offset) match
              case ')' => 3
              case ']' => 57
              case '}' => 1197
              case '>' => 25137
              case _   => 0
          case _ => 0
      }
      .compile
      .foldMonoid

  def part2(inputFile: Stream[IO, String]): IO[String] =

    def charScore(c: Char): Int = c match {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    }

    def calco(s: List[Char], acc: Long): Long = s match
      case Nil    => acc
      case h :: t => calco(t, acc * 5 + charScore(h))

    @tailrec
    def parsoo(
        s: List[Char],
        stack: List[Char]
    ): Either[(String, List[Char]), List[Char]] =
      s match
        case h :: t =>
          h match
            case l @ ('(' | '<' | '[' | '{') => parsoo(t, l :: stack)
            case ')' =>
              stack match
                case '(' :: tt => parsoo(t, tt)
                case o         => Left(s.mkString -> o)
            case '>' =>
              stack match
                case '<' :: tt => parsoo(t, tt)
                case o         => Left(s.mkString -> o)
            case ']' =>
              stack match
                case '[' :: tt => parsoo(t, tt)
                case o         => Left(s.mkString -> o)
            case '}' =>
              stack match
                case '{' :: tt => parsoo(t, tt)
                case o         => Left(s.mkString -> o)
        case Nil => Right(stack)

    def convert(i: Char): Char = i match
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case i   => i

    inputFile
      .filter(_.nonEmpty)
      .map { v =>
        parsoo(v.toCharArray.toList, Nil) match
          case Right(l) if l.nonEmpty =>
            v -> Some(l.map(convert))
          case _ => v -> None
      }
      .map(_._2)
      .unNone
      .map(l => calco(l, 0))
      .compile
      .toList
      .map(l => l.sorted.drop(l.size / 2).head.toString)
