import fpinscala.state.RNG.Simple
import fpinscala.testing.Gen

val s = Simple(42)

Gen.choose(5, 15).sample.run(s)

Gen.boolean.sample.run(s)
Gen.listOfN(5, Gen.boolean).sample.run(s)

Gen.tuple2(Gen.choose(0, 10)).sample.run(s)

Gen.listOfN(Gen.choose(1, 5)).sample.run(s)


Gen.union(
  Gen.choose(1, 5),
  Gen.choose(100, 150)
).sample.run(Simple(4))