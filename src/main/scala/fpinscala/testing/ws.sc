import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Simple
import fpinscala.testing.Gen

//val s = Simple(42)
//
//Gen.choose(5, 15).sample.run(s)
//
//Gen.boolean.sample.run(s)
//Gen.listOfN(5, Gen.boolean).sample.run(s)
//
//Gen.tuple2(Gen.choose(0, 10)).sample.run(s)
//
//Gen.listOfN(Gen.choose(1, 5)).sample.run(s)
//
//
//Gen.union(
//  Gen.choose(1, 5),
//  Gen.choose(100, 150)
//).sample.run(Simple(4))

import fpinscala.testing.Prop._
import fpinscala.testing.Gen._

//val smallInt = Gen.choose(-10, 10)
//val maxProp = forAll(listOf(smallInt)) { ns =>
//  val max = ns.max
//  !ns.exists(_ > max)
//}
//
//val maxProp2 = forAll(nonEmptyListOf(smallInt)) { ns =>
//  val max = ns.max
//  !ns.exists(_ > max)
//}
//
//run(maxProp)
//run(maxProp2)
//
//val sortProp = forAll(listOf(smallInt)) { ns =>
//  val maybeSorted = ns.sorted
//  if (maybeSorted.size < 2) true
//  else maybeSorted.sliding(2).foldLeft(true) {
//    case (res, x :: y :: _) if res => x <= y
//    case _ => false
//  }
//}
//
//run(sortProp)


import fpinscala.parallelism.Par

val ES: ExecutorService = Executors.newCachedThreadPool
val p1 = check(Par.map(Par.unit(1))(_ + 1)(ES).get == Par.unit(2)(ES).get)
run(p1)