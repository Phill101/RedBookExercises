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

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  val isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  val isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop((max, n, rng) => {
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  })

  def ||(p: Prop): Prop = Prop((max, n, rng) => {
    run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  })

  def tag(msg: String) = Prop {
    (maxSize, n, rng) => run(maxSize, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n$msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, n,rng) => randomStream(as)(rng).zipAll(Stream.from(0)).take(n).map {
      case (Some(a), Some(i)) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      case _ => sys.error("smth wrng")
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception) =
    s"""
       |test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}
     """.stripMargin
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
  def int: Gen[Int] = Gen(RNG.int)
  def char: Gen[Char] = Gen(RNG.nonNegativeLessThan(0xD800 - 1).map(x => (x + 1).toChar))
  def string: Gen[String] = Gen.listOfN(choose(0, 25), char).map(_.mkString)

  def listOf[A](g: Gen[A]): Gen[List[A]] = listOfN(choose(0, 10), g)
  def nonEmptyListOf[A](g: Gen[A]): Gen[List[A]] = listOfN(choose(1, 10), g)
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def listOfN[A](n: Gen[Int], g: Gen[A]): Gen[List[A]] = n.flatMap(listOfN(_, g))
  def listOfN(size: Gen[Int]): Gen[List[Int]] = size.flatMap(a => listOfN(a, size))

  def tuple2[A](g: Gen[A]): Gen[(A, A)] =
    Gen(for {
      f <- g.sample
      s <- g.sample
    } yield (f, s))

  def tuple2[A, B](g1: Gen[A], g2: Gen[B]): Gen[(A, B)] =
    Gen(for {
      f <- g1.sample
      s <- g2.sample
    } yield (f, s))

  def tuple3[A](g: Gen[A]): Gen[(A, A, A)] =
    Gen(for {
      f <- g.sample
      s <- g.sample
      t <- g.sample
    } yield (f, s, t))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    (g1, g2) match {
      case ((gen1, w1), (gen2, w2)) =>
        val ratio = w1.abs / (w1.abs + w2.abs)
        Gen(State(RNG.double).flatMap(d => if (d < ratio) gen1.sample else gen2.sample))
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = this.copy(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f.andThen(_.sample)))
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g(_).map(f))
  def flatMap[B](f: A => SGen[B]) = SGen(n => g(n).flatMap(f(_).g(n)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
}

