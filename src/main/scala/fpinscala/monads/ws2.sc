val f: Int => Some[Int] = i => Some(i + 1)
val g: Int => Some[Int] = i => Some(i * 2)
Some(1).flatMap(f).flatMap(g) == Some(1).flatMap(a => f(a).flatMap(g))
