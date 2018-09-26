package fpinscala.state

import fpinscala.state.State.sequence

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = new Rand(_.nextInt)

  def unit[A](a: A): Rand[A] = State.unit(a)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = s.map(f)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, nextRng) if i >= 0 => (i, nextRng)
      case (i, nextRng) if i != Int.MinValue => (i * -1, nextRng)
      case (i, nextRng) => ((i + 1) * -1, nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = State(nonNegativeInt).map(_ / (Int.MaxValue.toDouble + 1)).run(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (p, r) = intDouble(rng)
    (p.swap, r)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, State(double))

  def randDoubleInt: Rand[(Double, Int)] = both(State(double), int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f.flatMap(g)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(State(nonNegativeInt)) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}
