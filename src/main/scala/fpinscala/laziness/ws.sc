import fpinscala.laziness.Stream

val first = Stream(1, 2, 3, 4, 5, 6)
val second = Stream(1, 2, 3)

first.startsWith(second)

Stream.unfold(0)(s => if (s > 10) None else Option(s.toString, s+1)).toList

Stream.constant(2).take(10).toList

Stream.from(0).take(10).toList

Stream.fibs(0, 1).take(10).toList


Stream.map(first)(x => x.toString + "0 ").take(10).toList

Stream.take(first)(3).toList

Stream.takeWhile(first)(_ < 5).toList

first.zipAll(second).toList

first.takeWhile(_ < 4).toList

first.startsWith(second)

//Stream.hasSubsequence(first, second)