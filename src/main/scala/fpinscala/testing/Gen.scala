package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Simple

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def generateFun: State[RNG, Int] = {
      for {
        q <- RNG.nonNegativeLessThan(stopExclusive)
        r <- if (q >= start) State.unit[RNG, Int](q) else generateFun
      } yield r
    }

    Gen(generateFun)
  }

  def boolean: Gen[Boolean] = Gen(RNG.nonNegativeLessThan(2).map(i => if (i > 0) true else false))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def listOfN(size: Gen[Int]): Gen[List[Int]] = size.flatMap(a => listOfN(a, size) )

  def tuple2[A](g: Gen[A]): Gen[(A, A)] =
    Gen(for {
      f <- g.sample
      s <- g.sample
    } yield (f, s))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = choose(1, 3).flatMap(i => if (i < 2) g1 else g2)
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f.andThen(_.sample)))
}

trait SGen[+A] {

}

