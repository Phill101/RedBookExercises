import fpinscala.monads.Monad._
import fpinscala.testing.{Gen, Prop}

import scala.util.Try

val maybeInts = List("1", "2", "3")
val optionInts = maybeInts.map(i => Try(i.toInt).toOption)

optionMonad.sequence(optionInts)
optionMonad.traverse(maybeInts) { i =>
  Try(i.toInt).toOption
}

optionMonad.replicateM(5, Option(1))

listMonad.replicateM(5, List(1))
streamMonad.replicateM(5, Stream(1)).take(3).toList

optionMonad.product(Option(1), Option(2))
listMonad.product(List(1, 2), List(3, 4))


optionMonad.filterM(List(1, 2, 3, 4, 5)) { i =>
  if (i < 4) Option(true)
  else None
}

streamMonad.filterM(List(1, 2, 3, 4, 5)) { i =>
  if (i < 4) Stream(true)
  else Stream(false)
}

val g = genMonad.filterM(List(1, 2, 3)) { i =>
  Gen.boolean
}
