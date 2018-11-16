import fpinscala.applicative.{Monad, Traverse}

import scala.util.Try

val oMonad = new Monad[Option] {
  override def unit[A](a: => A) = Option(a)
  override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) =
    ma.flatMap(f)
}

val lMonad = new Monad[List] {
  override def unit[A](a: => A) = List(a)
  override def flatMap[A, B](ma: List[A])(f: A => List[B]) =
    ma.flatMap(f)
}

val tMonad = new Monad[Try] {
  override def unit[A](a: => A) = Try(a)
  override def flatMap[A, B](ma: Try[A])(f: A => Try[B]) =
    ma.flatMap(f)
}

val oTraverse = Traverse.optionTraverse

val composedMonad = Monad.composeM(lMonad, oMonad, oTraverse)

val one = composedMonad.unit(1)
val two = composedMonad.unit(2)

composedMonad.flatMap(one)(a =>
  List(Option(a + 1))
)
composedMonad.flatMap(two)(a =>
  List(Option(a + 1))
)