import fpinscala.state.State

/**
  * State это просто обёртка над функцией S => (A, S)
  * Мы применяем к некоторому S эту функцию, получаем A и НОВОЕ S.
  * Это удобно, когда таких State-action некоторое количество.
  * Внутри State определены flatMap и map,
  * позволяющие удобно комбинировать их в for-comprehension.
  *
  * Ниже мой пример с пирогом и его кусочками.
  * Это может быть и генератор случайных чисел (например), состояние которого (сид)
  * меняется после каждой новой генерации.
  */

case class PeaceOfCake()
case class Pie(peaces: List[PeaceOfCake])

// returns small portion (or nothing) and remaining part of a Pie
def getSmallPortion(p: Pie): (List[PeaceOfCake], Pie) =
  if (p.peaces.nonEmpty) (p.peaces.head :: Nil, Pie(p.peaces.tail))
  else (Nil, p)

def getMediumPortion(p: Pie): (List[PeaceOfCake], Pie) = {
  (for {
//    q <- State.get[Pie]         // returns p
//    _ <- State.set(Pie(Nil))    // sets p to Pie(Nil)
//    q <- State.modify[Pie](p => Pie(PeaceOfCake() :: p.peaces)) // append to state one more peace (state modification)
    p1 <- State(getSmallPortion)  // pass p to getSmallPortion function, that returns (result, new_state)
    p2 <- State(getSmallPortion)  // we pass new_state to getSmallPortion function, that returns (result, final_state)
  } yield p1 ++ p2).run(p)        // we combine our result (just for fun) and return (final_result, final_state)
}

val pie = Pie(List.fill(6)(PeaceOfCake()))

val small = State(getSmallPortion)
small.run(pie)


val medium = State(getMediumPortion)
medium.run(pie)


/**
  * Другой пример
  */
type Stack = List[Int]

def pop(s: Stack): (Int, Stack) =
  s match {
    case x :: xs => (x, xs)
    case Nil => sys.error("stack is empty")
  }

def push(a: Int)(s: Stack): (Unit, Stack) = ((), a :: s)

val stack = List(3, 4, 7, 1, 0)

(for {
  f <- State(pop)
  s <- State(pop)
  t <- State(pop)
  _ <- State(push(f + s + t))
  r <- State.get
} yield r).run(stack)