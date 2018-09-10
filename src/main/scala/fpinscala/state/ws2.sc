import fpinscala.state.RNG.Rand
import fpinscala.state.RNG.{Simple, int, ints, nonNegativeLessThan}
import fpinscala.state.State

val ns: Rand[List[Int]] =
  nonNegativeLessThan(10).flatMap(x =>
    int.flatMap(y =>
      ints(x).map(xs =>
        xs.map(_ % y))))

val g = Simple(42)

ns.run(g)

val nns: Rand[List[Int]] =
  for {
    x  <- nonNegativeLessThan(10)
    y  <- int
    xs <- ints(x)
  } yield xs.map(_ % y)

nns.run(g)