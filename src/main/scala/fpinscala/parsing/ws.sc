import fpinscala.parsing.JSON._
import fpinscala.parsing.{JSON, MyParsers}
import fpinscala.parsing.MyParsers.Parser

val myParsers = MyParsers.myParsers
import myParsers._

// useful
val spaces: Parser[String] = char(' ').many.slice
run(spaces)("     1")

val word: Parser[String] = "\\w+".r
run(word)("abra")

val number: Parser[Double] = regex("^[0-9]+(\\.[0-9]+)?$".r).map(_.toDouble)
run(number)("3241.87534")

val jstring: MyParsers.Parser[JString] =
  for {
    _ <- char('"')
    w <- word
    _ <- char('"')
  } yield JString(w)
run(jstring)("\"wow\"")

val jnumber: MyParsers.Parser[JNumber] =
  number.map(JNumber)
run(jnumber)("3241.87534")

val jbool: MyParsers.Parser[JBool] =
  word.map(b => JBool(b.toBoolean))
run(jbool)("true")

// tests
