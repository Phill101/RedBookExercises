import fpinscala.monads.{Id, Monad}
import fpinscala.state.State

import scala.language.{higherKinds, reflectiveCalls}

//Id("Hello, ") flatMap (a =>
//  Id("monad!") flatMap (b =>
//    Id(a + b)))

for {
  a <- Id("Hello, ")
  b <- Id("moand!")
} yield a + b

type IntState[A] = State[Int, A]

object IntStateMonad extends
  Monad[({type IntState[A] = State[Int, A]})#IntState] {

  override def unit[A](a: => A) = ???
  override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]) = ???
}

import Monad._

val l = Left[String, Int]("some error")
val r = Right[String, Int](1)
eitherMonad.map(r)(_ + 1)
eitherMonad.map(l)(_ + 1)


val q = stateMonad[Boolean].unit(1)
val w = stateMonad[Boolean].flatMap(q) { int =>
  State(s => if (s) (int + 1, false) else (int, s))
}
stateMonad.map2(q, w) { (f, s) =>
  f + s
}.run(true)
stateMonad.replicateM(5, w).run(true)
stateMonad.sequence(List(w, w, w)).run(true)

q.run(true)

val F = stateMonad[Int]
def zipWithIndex[A](as: List[A]): List[(Int, A)] =
  as.foldLeft(F.unit(List[(Int, A)]())) { (acc, a) =>
    for {
      xs <- acc
      n <- State.get
      _ <- State.set(n + 1)
    } yield (n, a) :: xs
  }.run(0)._1.reverse
zipWithIndex(List("one", "two"))