package fpinscala.monoids
import fpinscala.testing.{Gen, Prop}

object LawsTest extends App {
  val monoidLaws = Monoid.monoidLaws(Monoid.intAddition, Gen.int)

  Prop.run(monoidLaws)

  val productMonoidLaws = Monoid.monoidLaws(
    Monoid.productMonoid(Monoid.intAddition, Monoid.stringMonoid),
    Gen.tuple2(Gen.int, Gen.string)
  )
  Prop.run(productMonoidLaws)

}
