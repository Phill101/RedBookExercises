package fpinscala.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex

object MyParsers {
  type Parser[+A] = ParseState => Result[A]

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }

    /* Used by `flatMap` */
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommited: Boolean) extends Result[Nothing]

  case class ParseState(loc: Location) {
    def advancedBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))

    def input: String = loc.input.substring(loc.offset)

    def slice(n: Int): String = loc.input.substring(loc.offset, loc.offset + n)
  }

  val myParsers: Parsers[Parser] = new Parsers[Parser] {

    override implicit def string(s: String): Parser[String] = state => {
      val ensure = firstNonMatchingIndex(state.loc.input, s, state.loc.offset)
      if (ensure == -1)
        Success(s, s.length)
      else
        Failure(state.loc.toError(s"failed on $ensure char of '$s'"), ensure != 0)
    }

    override implicit def regex(r: Regex): Parser[String] = state => {
      r.findPrefixOf(state.input) match {
        case Some(v) => Success(v, v.length)
        case None => Failure(state.loc.toError("regex " + r), isCommited = false)
      }
    }

    override def slice[A](p: Parser[A]): Parser[String] = state => {
      p(state) match {
        case Success(_, i) => Success(state.loc.input.substring(0, i), 0)
        case f@Failure(_, _) => f
      }
    }

    override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = state => {
      p(state) match {
        case Success(a, i) => f(a)(state.advancedBy(i))
          .addCommit(i != 0)
          .advanceSuccess(i)
        case f@Failure(_, _) => f
      }
    }

    override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = state => {
      s1(state) match {
        case Failure(_, false) => s2(state)
        case r => r
      }
    }

    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
      val initialState = ParseState(Location(input))
      p(initialState).extract
    }

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def attempt[A](p: Parser[A]): Parser[A] = ???

    private def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
      var i = 0
      while (i < s1.length && i < s2.length) {
        if (s1.charAt(i + offset) != s2.charAt(i)) return i
        i += 1
      }
      if (s1.length - offset >= s2.length) -1
      else s1.length - offset
    }

    /* We provide an overridden version of `many` that accumulates
     * the list of results using a monolithic loop. This avoids
     * stack overflow errors for most grammars.
     */
    override def many[A](p: Parser[A]): Parser[List[A]] =
      s => {
        var nConsumed: Int = 0
        val buf = new collection.mutable.ListBuffer[A]

        def go(p: Parser[A], offset: Int): Result[List[A]] = {
          p(s.advancedBy(offset)) match {
            case Success(a, n) => buf += a; go(p, offset + n)
            case f@Failure(e, true) => f
            case Failure(e, _) => Success(buf.toList, offset)
          }
        }

        go(p, 0)
      }

  }
}

object Tests extends App {
  val myParsers = MyParsers.myParsers

  import myParsers._

  run(string("abra"))("abRa")
}