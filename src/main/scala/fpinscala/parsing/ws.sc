import fpinscala.parsing.JSON.{JNumber, JString}
import fpinscala.parsing.MyParsers
import fpinscala.parsing.MyParsers.Parser

val myParsers = MyParsers.myParsers
import myParsers._

val r1 = run(string("a"))("a")
val r2 = run(string("a"))("b")


// useful
val spaces: Parser[String] = char(' ').many.slice
run(spaces)("     1")

val quote: Parser[Char] = char('"')
run(quote)("\"")

val word: Parser[String] = "\\w+".r
run(word)("abra")

// todo it will not parse double
val number: Parser[Double] = regex("\\d+".r).map(_.toDouble)
run(number)("3241")

val jstring: MyParsers.Parser[JString] =
  for {
    _ <- quote
    w <- word
    _ <- quote
  } yield JString(w)
run(jstring)("\"wow\"")

val jnumber: MyParsers.Parser[JNumber] =
  number.map(JNumber)

// tests
run(string("abra"))("abra cadabra")
run(string("abra"))("abRa")

val abraOrCadabra = string("abra") | string("cadabra")
run(abraOrCadabra)("abra")
run(abraOrCadabra)("cadabra")

val abraDigit = string("abra") | """(\d+)""".r
run(abraDigit)("abra")
run(abraDigit)("123")

val a = char('a')
run(a)("abra")

val product = string("ab") ** string("abra")
run(product)("abra")

val many = string("a").many
run(many)("aaaabaa")

val sliced = many.slice
run(sliced)("aaaabaa")

val reg = "\\w+".r
run(reg)("wow")