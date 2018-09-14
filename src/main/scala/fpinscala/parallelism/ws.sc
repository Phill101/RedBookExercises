import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(sum(l), sum(r))(_ + _)
  }

sum(Array(1, 2, 3))

//def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

import fpinscala.parallelism.Par._
import java.util.concurrent.Executors
val es = Executors.newFixedThreadPool(4)

val units = List(
  unit(1), unit(2), unit(3), unit(4)
)

Par.sequence(units)(es)