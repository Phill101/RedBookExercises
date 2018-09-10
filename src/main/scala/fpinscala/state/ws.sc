import fpinscala.state.{RNG, State}
import fpinscala.state.RNG.Simple

val rng = Simple(42)

//val (n1, rng2) = rng.nextInt
//
//val (n2, rng3) = rng2.nextInt
//
//def randomPair(rng: RNG): ((Int, Int), RNG) = {
//  val (i1, rng2) = rng.nextInt
//  val (i2, rng3) = rng2.nextInt
//  ((i1, i2), rng3)
//}
//
//randomPair(rng)
//
//val (m1, r1) = RNG.nonNegativeInt(rng)
//val (m2, r2) = RNG.nonNegativeInt(r1)
//val (m3, r3) = RNG.nonNegativeInt(r2)
//val (m4, r4) = RNG.nonNegativeInt(r3)

//RNG.ints(3)(rng) // 16, -12, -34

//RNG.map(RNG.int)(_.toString)(rng)
//RNG.intDouble(rng)
//
//
//def rollDie: Rand[Int] = RNG.map(RNG.nonNegativeLessThan(6))(_ + 1)
//rollDie(Simple(5))

