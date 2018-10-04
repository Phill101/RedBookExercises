package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.testing.{Gen, Prop}
import fpinscala.testing.Prop.forAll

import language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def succeed[A](a: A): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def defaultSucceed[A](a: A): Parser[A] = string("").map(_ => a)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = for {
    r1 <- p
    r2 <- p2
  } yield (r1, r2)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(r1 => map(p2)(r2 => f(r1, r2)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def ~[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def label(msg: String): Parser[A] = ???
    def scope(msg: String): Parser[A] = ???
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](in: Gen[String]): Prop =
      forAll(in)(s => run(succeed("any"))(s) == Right("any"))
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}