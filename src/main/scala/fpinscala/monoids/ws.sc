import fpinscala.monoids.{ListFoldable, Monoid}

Monoid.foldLeft(List(1, 2, 3))("0") { (b, a) =>
  b + s" $a"
}

Monoid.foldRight(List(1, 2, 3))("0") { (a, b) =>
  a + s" $b"
}


Monoid.foldMapV(Array(1, 2, 3, 4), Monoid.stringMonoid)(_.toString)



Monoid.count("privet alo gblbl neploho")

val m1 = Map(1 -> "q", 2 -> "w")
val m2 = Map(1 -> "a", 2 -> "s", 3 -> "d")

val mmm = Monoid.mapMergeMonoid[Int, String](Monoid.stringMonoid)
mmm.op(m1, m2)


Monoid.bag(
  Array("roses", "are", "red", "as", "red", "roses")
)

import Monoid._

val pm = productMonoid(intAddition, intAddition)
val p = ListFoldable.foldMap(List(1, 2, 3, 4)) { a => (1, a)}(pm)