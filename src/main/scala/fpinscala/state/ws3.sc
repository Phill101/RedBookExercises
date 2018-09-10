import fpinscala.state.{Candy, Coin, Machine, Turn}

val m = Candy.simulateMachine(List(Coin, Turn, Coin, Turn))

m.run(Machine(locked = true, 5, 10))

import fpinscala.state.State._

get[Int].map(_ + 1).run(1)