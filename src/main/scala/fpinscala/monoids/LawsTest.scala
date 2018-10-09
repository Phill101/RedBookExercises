package fpinscala.monoids
import fpinscala.testing.{Gen, Prop}

object LawsTest extends App {
  val monoidLaws = Monoid.monoidLaws(Monoid.intAddition, Gen.int)

  Prop.run(monoidLaws)

}
