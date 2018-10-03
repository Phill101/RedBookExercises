package fpinscala.parsing

import scala.util.matching.Regex

object MyParsers {
  type Parser[+A] = ParseState => Result[A]

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e) => Left(e)
      case Success(a, _) => Right(a)
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  case class ParseState(loc: Location) {
    def advancedBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  val myParsers = new Parsers[Parser] {

    override implicit def string(s: String): Parser[String] = state => {
      val ensure = firstNonMatchingIndex(s, state.loc.input)
      if (ensure == -1)
        Success(s, s.length)
      else
        Failure(state.loc.toError(s"failed on $ensure char of '$s'"))
    }

    override implicit def regex(r: Regex): Parser[String] = ???

    override def slice[A](p: Parser[A]): Parser[String] = ???

    override def succeed[A](a: A): Parser[A] = ???

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

    override def or[A](s1: Parser[A], s2: =>Parser[A]): Parser[A] = ???

    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
      val initialState = ParseState(Location(input))
      p(initialState).extract
    }

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def attempt[A](p: Parser[A]): Parser[A] = ???

    private def firstNonMatchingIndex(input: String, state: String): Int = {
      def findDiffChar(inputTail: String, stateTail: String, i: Int): Int =
        if ((inputTail.isEmpty && stateTail.isEmpty) || (inputTail.isEmpty && stateTail.nonEmpty)) -1
        else if (inputTail.head == stateTail.head) findDiffChar(inputTail.tail, stateTail.tail, i+1)
        else i

      findDiffChar(input, state, 0)
    }
  }
}

object Tests extends App {
  val myParsers = MyParsers.myParsers
  import myParsers._

  run(string("abra"))("abRa")
}