import fpinscala.monoids.Monoid

Monoid.foldLeft(List(1, 2, 3))("0") { (b, a) =>
  b + s" $a"
}

Monoid.foldRight(List(1, 2, 3))("0") { (a, b) =>
  a + s" $b"
}


Monoid.foldMapV(Array(1, 2, 3, 4), Monoid.stringMonoid)(_.toString)



Monoid.count("privet alo gblbl neploho")