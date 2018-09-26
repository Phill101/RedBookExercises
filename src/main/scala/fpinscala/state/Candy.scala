package fpinscala.state

import fpinscala.state.State.{modify, sequence}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State.get
  def update(i: Input)(m: Machine): Machine =
    i match {
      case Coin if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
      case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      case _ => m
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      // апдейт кушает инпут возвращает машинку, после этого мы хотим к результату сразу же применить modify.
      // modify создаст новый стейт-экшн нужного типа(машинка).
      // sequence соберёт много стейт экшенов в один единственный стейт экшен. получаю машинку, отдаю лист.
      // в данном случае List[Unit], т.к. мы скомпозировали. Главное состояние передастся по цепочке flatMap!
      _ <- sequence(inputs.map((modify[Machine] _).compose(update))) // (modify[Machine] _).compose(update) - Input => State[Machine, Unit]
      s <- get // конечным будет возвращающий состояние стейт-экшн.
    } yield (s.coins, s.candies)
  }

}
