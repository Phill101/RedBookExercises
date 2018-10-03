import fpinscala.parsing.MyParsers

val myParsers = MyParsers.myParsers
import myParsers._

val r1 = run(string("a"))("a")
val r2 = run(string("a"))("b")



run(string("abra"))("abra cadabra")
run(string("abra"))("abRa")