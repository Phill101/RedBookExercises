import fpinscala.monads.{Id, Monad}
import fpinscala.state.State

import scala.language.reflectiveCalls

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

def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
  override def unit[A](a: => A) = State(s => (a, s))
  override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]) = ma.flatMap(f)
}

def eitherMonad[S] = new Monad[({type f[x] = Either[S, x]})#f] {
  override def unit[A](a: => A) = Right(a)
  override def flatMap[A, B](ma: Either[S, A])(f: A => Either[S, B]) = ma.flatMap(f)
}

val l = Left[String, Int]("some error")
val r = Right[String, Int](1)
eitherMonad.map(r)(_ + 1)
eitherMonad.map(l)(_ + 1)