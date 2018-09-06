package fpinscala.state

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

//  type Rand[+A] = RNG => (A, RNG)
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = new Rand(_.nextInt)

  def unit[A](a: A): Rand[A] = State.unit(a) // rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = s.map(f)
  //    rng => {
  //      val (a, rng2) = s(rng)
  //      (f(a), rng2)
  //    }
  //    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, nextRng) if i >= 0 => (i, nextRng)
      case (i, nextRng) if i != Int.MinValue => (i * -1, nextRng)
      case (i, nextRng) => ((i + 1) * -1, nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = State(nonNegativeInt).map(_ / (Int.MaxValue.toDouble + 1)).run(rng)
//    val (i, r) = nonNegativeInt(rng)
//    (i / (Int.MaxValue.toDouble + 1), r)
//    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (p, r) = intDouble(rng)
    (p.swap, r)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _)) // RNG.map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, State(double))

  def randDoubleInt: Rand[(Double, Int)] = both(State(double), int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
////    def go(remain: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
////      if (remain <= 0) (acc, r)
////      else {
////        val (i, nr) = r.nextInt
////        go(remain - 1, nr, i :: acc)
////      }
////    }
////    go(count, rng, Nil)
//    sequence(List.fill(count)(int))(rng)
//  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    rng =>
//      val (a, rng2) = ra(rng)
//      val (b, rng3) = rb(rng2)
//      (f(a, b), rng3)
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // unit(Nil) is the same as: rand => (Nil, rand)
    fs.foldRight[Rand[List[A]]](unit(Nil))((f, acc) => map2(f, acc)(_ :: _))
//    rand =>
//      val (l, nextRandom) = fs.foldLeft[(List[A], RNG)]((Nil, rand)) { case ((acc, crng), curRand) =>
//        val (v, nr) = curRand(crng)
//        (v :: acc, nr)
//      }
//      (l.reverse, nextRandom)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f.flatMap(g)
//    rand =>
//      val (a, r) = f(rand)
//      g(a)(r)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(State(nonNegativeInt)) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a))) // flatMap(s)(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b))) //  flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(st => {
      val (a, s) = run(st)
      f(a).run(s)
    })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(rng => (a, rng))
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
